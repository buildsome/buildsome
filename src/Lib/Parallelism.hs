{-# LANGUAGE OverloadedStrings #-}
module Lib.Parallelism
  ( ParId
  , Pool, newPool
  , Priority(..)
  , Fork(..), fork, wrapForkedChild
  , TokenCell, rootTokenCell
  , withReleased
  ) where

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

type ParId = Int
type Pool = PoolAlloc ParId

data TokenCellState
    = TokenCellAlloced ParId
    | TokenCellAllocating (MVar ())
    | TokenCellReleasedToChildren Int
    | TokenCellFinished -- released back to parent

newtype TokenCell = TokenCell
    { tokenCellState :: IORef TokenCellState
    }

data WaitContext = WaitContext
    { wcPriority :: Priority
    , wcChildCount :: IORef Int
    , wcParentAlloc :: MVar (Alloc ParId)
    }

data ForkState = ForkStarted [WaitContext] | ForkDone

data Fork = Fork
    { forkPool :: Pool
    , forkState :: IORef ForkState
    , -- This is only valid between the fork call and the
      -- wrapForkedChild
      forkAlloc :: Alloc ParId
    }
instance Eq Fork where
    (==) = (==) `on` forkState -- compare ioref identities

newPool :: ParId -> IO Pool
newPool n = PoolAlloc.new [1..n]

rootTokenCell :: Pool -> IO TokenCell
rootTokenCell pool =
    do
        state <- newIORef . TokenCellAlloced =<< PoolAlloc.alloc (Priority 0) pool
        return $ TokenCell state

waitContextDone :: PoolAlloc ParId -> WaitContext -> IO ()
waitContextDone pool wc =
    do
        -- Allocate on behalf of parent
        parentAlloc <- PoolAlloc.startAlloc (wcPriority wc) pool
        putMVar (wcParentAlloc wc) parentAlloc

cancelParentAllocation :: Pool -> WaitContext -> IO ()
cancelParentAllocation pool wc =
    do
        -- The children were unlinked, so nobody can fill the mvar
        -- anymore, so this is not a race:
        mAlloc <- tryTakeMVar (wcParentAlloc wc)
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

notifyParentChildDone :: PoolAlloc ParId -> WaitContext -> IO ()
notifyParentChildDone pool wc =
    join $ atomicModifyIORef (wcChildCount wc) $
    \n -> (n - 1, refCountUpdated (n - 1))
    where
        refCountUpdated n
            | n > 0 = return ()
            | n == 0 = waitContextDone pool wc
            | otherwise = error "Negative refcount?!"

childDone :: PoolAlloc ParId -> Fork -> IO ()
childDone pool child =
    join $ atomicModifyIORef (forkState child) $ \oldState ->
    case oldState of
    ForkStarted parents ->
        ( ForkDone
        , mapM_ (notifyParentChildDone pool) parents
        )
    ForkDone -> error "Child done twice?!"

linkChild :: Pool -> WaitContext -> Fork -> IO ()
linkChild pool wc child =
    join $ atomicModifyIORef (forkState child) $ \oldState ->
    case oldState of
    ForkStarted parents -> (ForkStarted (wc:parents), return ())
    ForkDone -> (ForkDone, notifyParentChildDone pool wc)

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
                , (takeMVar (wcParentAlloc wc) >>= PoolAlloc.finish >>= setAlloced)
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

withReleased :: Pool -> TokenCell -> Priority -> [Fork] -> IO a -> IO a
withReleased pool token priority dupChildren action =
    do  mvar <- newEmptyMVar
        childCount <- newIORef (length children)
        let wc = WaitContext
                { wcPriority = priority
                , wcChildCount = childCount
                , wcParentAlloc = mvar
                }
            beforeRelease =
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
            afterRelease =
                regainToken token wc
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
                fmap TokenCell . newIORef $ TokenCellAlloced parId
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
        return Fork
            { forkPool = pool
            , forkState = state
            , forkAlloc = alloc
            }
