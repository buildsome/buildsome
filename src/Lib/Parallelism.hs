{-# LANGUAGE OverloadedStrings, CPP #-}
module Lib.Parallelism
  ( ParId
  , Pool, newPool
  , Priority(..)
  , Fork(..), fork, wrapForkedChild, forkBoostPriority
  , TokenCell, rootTokenCell
  , withReleased
  ) where

-- TODO: Verify/guarantee that priorities of deps are always given as
-- higher than parent

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (void, join)
import           Data.Function (on)
import           Data.IORef
import qualified Data.List as List
import           Lib.Exception (bracket, bracket_, finally, onException)
import           Lib.IORef (atomicModifyIORef_)
import           Lib.PoolAlloc (PoolAlloc, Priority(..), Alloc)
import qualified Lib.PoolAlloc as PoolAlloc

#if __GLASGOW_HASKELL__ <= 706
-- missing tryReadMVar
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar mvar = bracket (tryTakeMVar mvar) (maybe (return ()) (putMVar mvar)) return
#endif

type ParId = Int
type Pool = PoolAlloc ParId

data TokenCellState
    = TokenCellAlloced ParId
    | TokenCellAllocating (MVar ())
    | TokenCellReleasedToChildren Int
    | TokenCellFinished -- released back to parent

data TokenCell = TokenCell
    { tokenCellState :: IORef TokenCellState
    , tokenCellFrom :: Maybe Fork
    }

data WaitContextState
    = WaitContextWaiting
    | WaitContextAllocating

data WaitContext = WaitContext
    { wcState :: IORef (WaitContextState, Priority) -- becomes Nothing when already read and allocation starts
    , wcChildren :: IORef [Fork]
    , wcParentAlloc :: MVar (Alloc ParId)
    }

data ForkState = ForkStarted [WaitContext] | ForkDone

data Fork = Fork
    { forkPool :: Pool
    , forkState :: IORef ForkState
    , forkAlloc :: Alloc ParId
    , forkPriority :: IORef Priority
    , forkWCs :: IORef [WaitContext]
    }
instance Eq Fork where
    (==) = (==) `on` forkState -- compare ioref identities

newPool :: ParId -> IO Pool
newPool n = PoolAlloc.new [1..n]

rootTokenCell :: Pool -> IO TokenCell
rootTokenCell pool =
    do
        state <- newIORef . TokenCellAlloced =<< PoolAlloc.alloc (Priority 0) pool
        return $ TokenCell
            { tokenCellState = state
            , tokenCellFrom = Nothing
            }

waitContextDone :: PoolAlloc ParId -> WaitContext -> IO ()
waitContextDone pool wc =
    do
        -- Allocate on behalf of parent
        priority <-
            atomicModifyIORef (wcState wc) $
            \(WaitContextWaiting, p) -> ((WaitContextAllocating, p), p)
        parentAlloc <- PoolAlloc.startAlloc priority pool
        putMVar (wcParentAlloc wc) parentAlloc

cancelParentAllocation :: Pool -> WaitContext -> IO ()
cancelParentAllocation pool wc =
    do
        -- The children were unlinked, so nobody can fill the mvar
        -- anymore, so this is not a race:
        mAlloc <- tryReadMVar (wcParentAlloc wc)
        case mAlloc of
            Nothing ->
                -- children didn't start alloc for us, nothing to do
                return ()
            Just alloc ->
                -- children started alloc for us before we managed to
                -- unlink them. There's no facility to cancel a
                -- started allocation, but we can at least detach it
                -- to a background thread that just allocates to
                -- release:
                void $ forkIO $ PoolAlloc.finish alloc >>= PoolAlloc.release pool

notifyParentChildDone :: PoolAlloc ParId -> Fork -> WaitContext -> IO ()
notifyParentChildDone pool child wc =
    join $ atomicModifyIORef (wcChildren wc) $
    \children ->
    case List.partition (== child) children of
    ([_], []) -> ([], waitContextDone pool wc)
    ([_], rest) -> (rest, return ())
    _ -> error "Expecting to find child WC exactly once in list of children"

childDone :: PoolAlloc ParId -> Fork -> IO ()
childDone pool child =
    join $ atomicModifyIORef (forkState child) $ \oldState ->
    case oldState of
    ForkStarted parents ->
        ( ForkDone
        , mapM_ (notifyParentChildDone pool child) parents
        )
    ForkDone -> error "Child done twice?!"

linkChild :: Pool -> WaitContext -> Fork -> IO ()
linkChild pool wc child =
    join $ atomicModifyIORef (forkState child) $ \oldState ->
    case oldState of
    ForkStarted parents -> (ForkStarted (wc:parents), return ())
    ForkDone -> (ForkDone, notifyParentChildDone pool child wc)

unlinkChild :: WaitContext -> Fork -> IO ()
unlinkChild wc child =
    atomicModifyIORef_ (forkState child) $ \oldState ->
    case oldState of
    ForkStarted parents ->
        ForkStarted $ filter ((myMVar /=) . wcParentAlloc) parents
    ForkDone -> ForkDone
    where
        myMVar = wcParentAlloc wc

modifyTokenState :: TokenCell -> (TokenCellState -> (TokenCellState, IO a)) -> IO a
modifyTokenState token = join . atomicModifyIORef (tokenCellState token)

releaseToken :: Pool -> TokenCell -> IO ()
releaseToken pool token =
    modifyTokenState token $
    \state ->
    case state of
    TokenCellAlloced parId ->
        (TokenCellReleasedToChildren 0, PoolAlloc.release pool parId)
    TokenCellReleasedToChildren n ->
        (TokenCellReleasedToChildren (n + 1), return ())
    TokenCellAllocating mvar -> (state, readMVar mvar >> releaseToken pool token)
    TokenCellFinished -> error "TokenCellFinished???"

regainToken :: TokenCell -> WaitContext -> IO ()
regainToken token wc =
    do
        mvar <- newEmptyMVar
        modifyTokenState token $ \state ->
            case state of
            TokenCellReleasedToChildren 0 ->
                ( TokenCellAllocating mvar
                , (readMVar (wcParentAlloc wc) >>= PoolAlloc.finish >>= setAlloced)
                  `finally` putMVar mvar ()
                )
            TokenCellReleasedToChildren n ->
                ( TokenCellReleasedToChildren (n-1), return () )
            _ -> error "regain at invalid state"
    where
        setAlloced parId =
            modifyTokenState token $ \state ->
            case state of
            TokenCellAllocating {} -> (TokenCellAlloced parId, return ())
            _ -> error "Token state changed underneath us?!"

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and regained once. The regain will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

wcBoostPriority :: Priority -> WaitContext -> IO ()
wcBoostPriority priority wc =
    do
        needsUpdate <-
            join $ atomicModifyIORef (wcState wc) $
            \state ->
            case state of
            (_, oldPriority)
                | priority <= oldPriority -> (state, return False)
            (WaitContextWaiting, _) ->
                ( (WaitContextWaiting, priority)
                , return True -- the alloc will use the new priority, we're done
                )
            (WaitContextAllocating, _) ->
                ( (WaitContextAllocating, priority)
                , do
                    alloc <- readMVar (wcParentAlloc wc)
                    PoolAlloc.changePriority alloc priority
                    return True
                )
        when needsUpdate $
            readIORef (wcChildren wc) >>= mapM_ (forkBoostPriority priority)

addWC :: WaitContext -> TokenCell -> IO ()
addWC wc token =
    case tokenCellFrom token of
    Nothing -> return ()
    Just parentFork ->
        do
            atomicModifyIORef_ (forkWCs parentFork) (wc:)
            newForkPriority <- readIORef (forkPriority parentFork)
            -- if we added the wc right after a fork priority boost,
            -- it may have missed our addition, but then we know the
            -- fork priority is necessarily already set, so read that
            -- to see if boost needed
            wcBoostPriority newForkPriority wc

withReleased :: Pool -> TokenCell -> Priority -> [Fork] -> IO a -> IO a
withReleased pool token priority dupChildren action =
    do  mvar <- newEmptyMVar
        childrenRef <- newIORef children
        stateRef <- newIORef $ (WaitContextWaiting, priority)
        let wc = WaitContext
                { wcState = stateRef
                , wcChildren = childrenRef
                , wcParentAlloc = mvar
                }
        addWC wc token
        let beforeRelease =
                do
                    -- Bracket's mask is enough here, this is non-blocking:
                    mapM_ (linkChild pool wc) children
                    -- However, this is blocking (on a parallel
                    -- allocation of the same TokenCell). Instead of
                    -- handling an exception here, we assume whoever
                    -- blocks us by allocating is properly
                    -- interruptible
                    releaseToken pool token
                        `onException`
                        do
                            mapM_ (unlinkChild wc) children
                            cancelParentAllocation pool wc
        let afterRelease = regainToken token wc
        bracket_ beforeRelease afterRelease action
    where
        children = List.nub dupChildren

-- Must run masked (so allocation gets a chance to run)
wrapForkedChild :: Fork -> (TokenCell -> IO r) -> IO r
wrapForkedChild child =
    bracket (beforeChild `onException` cancelFork) afterChild
    where
        pool = forkPool child
        cancelFork = childDone pool child
        beforeChild =
            do
                -- This blocks and may be interruptible legitimately
                -- by an exception and then cancelFork will make sure
                -- nobody waits for us forever
                parId <- PoolAlloc.finish $ forkAlloc child
                cellState <- newIORef $ TokenCellAlloced parId
                return TokenCell
                    { tokenCellState = cellState
                    , tokenCellFrom = Just child
                    }
        afterChild token =
            modifyTokenState token $
            \oldState ->
            case oldState of
            TokenCellAlloced parId ->
                ( TokenCellFinished
                , do
                    childDone pool child
                    PoolAlloc.release pool parId
                )
            _ -> error "forked child did not return to Alloced state"

-- | Must call wrapForkedChild at the child context!
-- Must mask this call to guarantee the above!
fork :: Pool -> Priority -> IO Fork
fork pool priority =
    do
        alloc <- PoolAlloc.startAlloc priority pool
        state <- newIORef $ ForkStarted []
        priorityRef <- newIORef priority
        wcs <- newIORef []
        return Fork
            { forkPool = pool
            , forkState = state
            , forkAlloc = alloc
            , forkPriority = priorityRef
            , forkWCs = wcs
            }

-- | Boost the priority of a fork and all of its dependencies to at
-- least the given priority. This assumes dependencies of all forks
-- are always at least as high in their priority as their parent.
forkBoostPriority :: Priority -> Fork -> IO ()
forkBoostPriority priority f =
    do
        curPriority <- readIORef $ forkPriority f
        when (priority > curPriority) $
            writeIORef (forkPriority f) priority
            `finally`
            do
                PoolAlloc.changePriority (forkAlloc f) priority
                mapM_ (wcBoostPriority priority) =<< readIORef (forkWCs f)
