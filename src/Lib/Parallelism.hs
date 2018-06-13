{-# LANGUAGE CPP #-}
module Lib.Parallelism
  ( ParId
  , Pool, newPool
  , Entity, rootEntity
  , fork
  , withReleased
  , upgradePriority
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (join, when, void)
import           Data.Foldable (traverse_)
import           Data.Function (on)
import           Data.IORef
import qualified Data.List as List
import           Lib.Exception (bracket_, onException)
import           Lib.PoolAlloc (PoolAlloc, Alloc)
import qualified Lib.PoolAlloc as PoolAlloc
import           Lib.Printer (Printer)

import           Prelude.Compat

type ParId = Int
type Pool = PoolAlloc ParId

data Priority = Priority
    { priorityIsUpgraded :: Bool -- True(+1) or (+0) of to max of parents?
    , priorityMaxOfParents :: PoolAlloc.Priority -- current (+1) or (+0) of max of parents
    } deriving (Show)

priorityVal :: Priority -> PoolAlloc.Priority
priorityVal (Priority False val) = val
priorityVal (Priority True val) = 1+val

data EntityRunningState
    = EntityForking !(Alloc ParId)
    | EntityAlloced !ParId
    | EntityReleasedToChildren
      {-children:       -}![Entity]
      {-Moved to EntityReallocating: -}!(MVar (Alloc ParId))
    | EntityReallocStarting {-Moved to EntityReallocating: -}!(MVar (Alloc ParId))
      -- ^ Intermediate state towards EntityReallocating
    | EntityReallocating !(Alloc ParId)
      {-Alloc complete: -}!(MVar ParId)

ersDebugShow :: EntityRunningState -> String
ersDebugShow EntityForking{}            = "EntityForking"
ersDebugShow EntityAlloced{}            = "EntityAlloced"
ersDebugShow EntityReleasedToChildren{} = "EntityReleasedToChildren"
ersDebugShow EntityReallocStarting{}    = "EntityReallocStarting"
ersDebugShow EntityReallocating{}       = "EntityReallocating"

data EntityRunning = EntityRunning
    { entityRunningParents :: [Entity]
    , entityRunningPriority :: !Priority
    , entityRunningReleaseCounter :: !Int
    , entityRunningState :: EntityRunningState
    }

data EntityState
    = EntityStateRunning EntityRunning
    | EntityStateFinished

data Entity = Entity
    { entityState :: !(IORef EntityState)
    , entityId :: String
    }
instance Eq Entity where
    (==) = (==) `on` entityState -- compare ioref identities

newPool :: ParId -> IO Pool
newPool n = PoolAlloc.new [1..n]

rootEntity :: Pool -> IO Entity
rootEntity pool =
    do
        parId <- PoolAlloc.alloc 0 pool
        state <-
            newIORef $ EntityStateRunning EntityRunning
            { entityRunningParents = []
            , entityRunningPriority = Priority False 0
            , entityRunningState = EntityAlloced parId
            , entityRunningReleaseCounter = 0
            }
        pure Entity
            { entityState = state
            , entityId = "Root"
            }

modifyEntityState :: Entity -> (EntityState -> (EntityState, IO a)) -> IO a
modifyEntityState entity = join . atomicModifyIORef (entityState entity)

modifyEntityRunningState ::
    Entity ->
    (EntityRunning -> EntityRunningState -> (EntityRunningState, IO r)) -> IO r
modifyEntityRunningState entity f =
    modifyEntityState entity $
    \case
    EntityStateRunning running ->
        let (newRunningState, action) = f running (entityRunningState running)
        in ( EntityStateRunning running { entityRunningState = newRunningState }
           , action
           )
    EntityStateFinished -> error "Expecting a running entity"

-- | Avoid potentially blocking putMVar as all MVars here are only ever put once
assertPutMVar :: MVar a -> a -> IO ()
assertPutMVar mvar val =
    do
        True <- tryPutMVar mvar val
        pure ()

-- runs under mask (non-blocking)
allChildrenDone :: Printer -> PoolAlloc.Priority -> Pool -> Entity -> IO ()
allChildrenDone _printer priority pool parent =
    do
        alloc <- PoolAlloc.startAlloc priority pool
        allocCompletionMVar <- newEmptyMVar
        modifyEntityRunningState parent $ \running ->
            \case
            EntityReallocStarting allocStartedMVar ->
              ( EntityReallocating alloc allocCompletionMVar
              , do
                    let priority' = priorityVal (entityRunningPriority running)
                    when (priority' > priority) $
                        PoolAlloc.changePriority alloc priority'
                    assertPutMVar allocStartedMVar alloc
              )
            state -> error $ "Expecting to be in EntityReallocStarting, but we're in: " ++ ersDebugShow state

-- runs under mask (non-blocking)
notifyParent :: Printer -> Pool -> Entity -> Entity -> IO ()
notifyParent printer pool child parent =
    modifyEntityRunningState parent $
    \running -> \case
      EntityReleasedToChildren children allocStartedMVar ->
        case List.partition (== child) children of
        ([_], []) ->
            -- last child is finished, we can realloc the token for the parent
            ( EntityReallocStarting allocStartedMVar
            , allChildrenDone printer
              (priorityVal (entityRunningPriority running)) pool parent
            )
        ([_], rest) ->
            ( EntityReleasedToChildren rest allocStartedMVar
            , pure ()
            )
        ([], _) ->
            error $
            "notifyParent: Missing child out of " ++ show (length children) ++
            " children"
        (cs@(_:_:_), _) ->
            error $
            "notifyParent: Finding " ++ show (length cs) ++
            " (duplicate) children out of " ++ show (length children) ++ " children"
      state -> error $ "notifyParent: Invalid state: " ++ ersDebugShow state ++ "?!"

-- runs under mask (non-blocking)
linkChild :: Printer -> Pool -> Entity -> Entity -> IO ()
linkChild printer pool parent child =
    modifyEntityState child $
    \case
    EntityStateFinished -> (EntityStateFinished, notifyParent printer pool child parent)
    EntityStateRunning er ->
        ( EntityStateRunning er { entityRunningParents = parent:entityRunningParents er }
        , pure ()
        )

cancelAllocation :: Pool -> Alloc ParId -> IO ()
cancelAllocation pool alloc =
    -- No proper cancellation support yet, so just finish and release in a thread
    void $ forkIO $ E.uninterruptibleMask_ $ PoolAlloc.finish alloc >>= PoolAlloc.release pool

{-# INLINE onFirst #-}
onFirst :: (a -> a') -> (a, b) -> (a', b)
onFirst f (x, y) = (f x, y)

maybeReleaseToChildrenLoop ::
    MVar (Alloc ParId) -> Printer -> Pool -> Entity -> [Entity] -> IO ()
maybeReleaseToChildrenLoop allocStartedMVar printer pool parent children =
    modifyEntityState parent $
    \case
      EntityStateFinished -> error "maybeReleaseToChildrenLoop: invalid state: EntityStateFinished?!"
      EntityStateRunning (EntityRunning parents priority rc runState) ->
        let linkBoost child =
                do
                    boostChildPriority printer (priorityVal priority) child
                    linkChild printer pool parent child
        in
        onFirst (EntityStateRunning . uncurry (EntityRunning parents priority)) $
        case runState of
        EntityForking {} -> error "withReleased must be invoked in a child context"

        EntityAlloced parId | rc == 0 ->
            ( (rc + 1, EntityReleasedToChildren children allocStartedMVar)
            , do
                -- The following don't throw exceptions and are non-blocking (we're not interruptible)
                traverse_ linkBoost children
                PoolAlloc.release pool parId
            )
        EntityAlloced {} -> error "RC != 0 but we're at alloced?!"

        EntityReallocStarting mvar ->
            ( (rc, EntityReallocStarting mvar)
            , -- blocking readMVar may throw async exception, but
              -- that is a no-op at this point, so is fine
              readMVar mvar >> loop
            )

        EntityReallocating alloc mvar | rc == 0 ->
            ( (rc, EntityReallocating alloc mvar)
            , -- ditto about blocking
              readMVar mvar >> loop
            )

        -- All children are done:
        EntityReallocating alloc allocCompletionMVar ->
            ( (rc + 1, EntityReleasedToChildren children allocStartedMVar)
              -- TODO: readMVar allocStartedMVar >> loop
            , do
                traverse_ linkBoost children
                assertPutMVar allocCompletionMVar $ error $
                    "Only releaseToChildren(rc == 0) should read this " ++
                    "mvar but we increased counter so it should not be " ++
                    "invoked! reallocFromChildren should put to this " ++
                    "mvar but it too should not be invoked"
                -- The following don't throw exceptions and are non-blocking (we're not interruptible)
                cancelAllocation pool alloc
            )

        EntityReleasedToChildren {} | rc == 0 -> error "Cannot be releaseToChildren when got token!"
        EntityReleasedToChildren oldChildren oldAllocStartedMVar ->
            ( ( rc + 1
              , EntityReleasedToChildren (uniqueChildren ++ oldChildren) oldAllocStartedMVar
              )
            , traverse_ linkBoost uniqueChildren
            )
            where
                uniqueChildren = filter (`notElem` oldChildren) children
        where
            loop = maybeReleaseToChildrenLoop allocStartedMVar printer pool parent children

-- Runs under mask
-- Interruptible, but exception-safe (no effect if async exception)
maybeReleaseToChildren :: Printer -> Pool -> Entity -> [Entity] -> IO ()
maybeReleaseToChildren printer pool parent children =
    do
        allocStartedMVar <- newEmptyMVar
        maybeReleaseToChildrenLoop allocStartedMVar printer pool parent children

-- Runs under uninterruptibleMask, thus exception-safe
maybeReallocFromChildren :: Printer -> Entity -> IO ()
maybeReallocFromChildren _printer parent =
    modifyEntityState parent $
    \case
      EntityStateFinished -> error "maybeReallocFromChildren: invalid state: EntityStateFinished?!"
      EntityStateRunning running ->
        case running of
        EntityRunning _ _ 0 _ -> error "Realloc when rc is 0?!"
        EntityRunning parents priority 1 runState ->
            ( EntityStateRunning (EntityRunning parents priority 0 runState)
            , case runState of
                EntityReleasedToChildren _ allocStartedMVar ->
                    readMVar allocStartedMVar >>= finishAlloc
                EntityReallocStarting allocStartedMVar ->
                    readMVar allocStartedMVar >>= finishAlloc
                EntityReallocating alloc _ ->
                    finishAlloc alloc
                _ -> error "realloc at invalid state"
            )
        EntityRunning parents priority rc runState ->
            ( EntityStateRunning (EntityRunning parents priority (rc - 1) runState)
            , pure ()
            )
        where
            finishAlloc alloc =
                do
                    parId <- PoolAlloc.finish alloc
                    modifyEntityRunningState parent $ \_running state ->
                        case state of
                        EntityReallocating _alloc allocCompleteMVar ->
                            (EntityAlloced parId, assertPutMVar allocCompleteMVar parId)
                        _ -> error "Entity state changed underneath us?!"

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and realloced once. The realloc will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

withReleased :: Printer -> Pool -> Entity -> [Entity] -> IO r -> IO r
withReleased printer pool parent dupChildren =
    bracket_
    (maybeReleaseToChildren printer pool parent children)
    (maybeReallocFromChildren printer parent)
    where
        children = List.nub dupChildren

-- | Must use the resulting wrapper on the child (which implies
-- masking this call)!
fork :: String -> Printer -> Pool -> IO (Entity, Printer -> IO a -> IO a)
fork ident _printer pool =
    do
        alloc <- PoolAlloc.startAlloc 0 pool
        stateRef <-
            newIORef $ EntityStateRunning
            EntityRunning
            { entityRunningParents = []
            , entityRunningPriority = Priority False 0
            , entityRunningReleaseCounter = 0
            , entityRunningState = EntityForking alloc
            }
        let entity =
                Entity
                { entityState = stateRef
                , entityId = ident
                }
        pure (entity, wrapChild pool entity alloc)

-- Called only if the fork allocation has failed!
-- Runs under uninterruptibleMask
cancelFork :: Printer -> Pool -> Entity -> IO ()
cancelFork printer pool child =
    modifyEntityState child $
    \case
    EntityStateRunning
        (EntityRunning parents _priority 0 (EntityForking _alloc)) ->
            ( EntityStateFinished
            , traverse_ (notifyParent printer pool child) parents
            )
    _ -> error "cancelFork: Expecting an EntityForking with rc=0"


-- Must run masked (implied by wrapChild being guaranteed to run)
wrapChild :: Pool -> Entity -> Alloc ParId -> Printer -> IO a -> IO a
wrapChild pool child alloc printer =
    bracket_
    (beforeChild `onException` cancelFork printer pool child)
    afterChild
    where
        beforeChild =
            do
                parId <- PoolAlloc.finish alloc
                modifyEntityRunningState child $ \_running ->
                  \case
                  EntityForking _alloc -> (EntityAlloced parId, pure ())
                  state -> error $ "wrapChild/before: invalid state: " ++ ersDebugShow state ++ "?!"
        afterChild =
            -- runs under uninterruptibleMask
            modifyEntityState child $
            \case
            EntityStateRunning
                (EntityRunning parents _ 0 (EntityAlloced parId)) ->
                    ( EntityStateFinished
                    , do
                        traverse_ (notifyParent printer pool child) parents
                        PoolAlloc.release pool parId
                    )
            _ -> error "forked child did not pure to Alloced state with rc=0"

boostChildPriority :: Printer -> PoolAlloc.Priority -> Entity -> IO ()
boostChildPriority printer parentVal =
    boostPriority printer $ \p -> p { priorityMaxOfParents = parentVal }

boostPriority :: Printer -> (Priority -> Priority) -> Entity -> IO ()
boostPriority printer upgrade entity =
    modifyEntityState entity $
    \state ->
    case state of
    EntityStateFinished -> (state, pure ())
    EntityStateRunning (EntityRunning parents oldPriority rc runState)
        | priorityVal newPriority <= priorityVal oldPriority -> (state, pure ())
        | otherwise ->
            ( EntityStateRunning (EntityRunning parents newPriority rc runState)
            , case runState of
              EntityForking alloc ->
                  PoolAlloc.changePriority alloc (priorityVal newPriority)
              EntityReallocating alloc _mvar ->
                  PoolAlloc.changePriority alloc (priorityVal newPriority)
              EntityReleasedToChildren children _mvar ->
                  traverse_ (boostChildPriority printer (priorityVal newPriority)) children
              EntityReallocStarting _ -> pure ()
              -- Not waiting for children, not allocating, do nothing:
              EntityAlloced _ -> pure ()
            )
        where
            newPriority = upgrade oldPriority

upgradePriority :: Printer -> Entity -> IO ()
upgradePriority printer entity =
    E.mask_ $ boostPriority printer upgrade entity
    where
        upgrade p = p { priorityIsUpgraded = True }
