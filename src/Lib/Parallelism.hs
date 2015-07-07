{-# LANGUAGE OverloadedStrings, CPP #-}
module Lib.Parallelism
  ( ParId
  , Pool, newPool
  , Priority(..)
  , Entity, rootEntity
  , fork
  , boostPriority
  , withReleased
  ) where

-- TODO: Verify/guarantee that priorities of deps are always given as
-- higher than parent

import           Control.Concurrent.MVar
import           Control.Monad (void, join)
import           Data.Function (on)
import           Data.IORef
import qualified Data.List as List
import           Lib.Exception (bracket_, onException)
import           Lib.IORef (atomicModifyIORef_)
import           Lib.PoolAlloc (PoolAlloc, Priority(..), Alloc)
import qualified Lib.PoolAlloc as PoolAlloc

type ParId = Int
type Pool = PoolAlloc ParId

data EntityState
    = EntityForking !Priority !(Alloc ParId)
    | EntityReallocating !Priority !(Alloc ParId)
      {-Alloc complete: -}!(MVar ParId)
    | EntityAlloced !ParId
    | EntityReleasedToChildren
      {-will realloc at: -}!Priority
      {-children:       -}![Entity]
      {-Moved to EntityReallocating: -}!(MVar (Alloc ParId))
    | EntityReallocStarting !Priority {-Moved to EntityReallocating: -}!(MVar (Alloc ParId))
      -- ^ Intermediate state towards EntityReallocating
    | EntityFinished -- released back to parent

data EntityNotifyParents
    = EntityWillNotify {-parents-}[Entity]
    | EntityAlreadyNotified

data Entity = Entity
    { entityState :: !(IORef EntityState)
    , entityReleaseCounter :: !(IORef Int)
    , entityNotifyParents :: !(IORef EntityNotifyParents)
    }
instance Eq Entity where
    (==) = (==) `on` entityState -- compare ioref identities

newPool :: ParId -> IO Pool
newPool n = PoolAlloc.new [1..n]

rootEntity :: Priority -> Pool -> IO Entity
rootEntity priority pool =
    do
        state <- newIORef . EntityAlloced =<< PoolAlloc.alloc priority pool
        notifyParents <- newIORef $ EntityWillNotify []
        releaseCounterRef <- newIORef 0
        return $ Entity
            { entityState = state
            , entityNotifyParents = notifyParents
            , entityReleaseCounter = releaseCounterRef
            }

modifyEntityState :: Entity -> (EntityState -> (EntityState, IO a)) -> IO a
modifyEntityState entity = join . atomicModifyIORef (entityState entity)

modifyEntityState_ :: Entity -> (EntityState -> EntityState) -> IO ()
modifyEntityState_ = atomicModifyIORef_ . entityState

-- runs under mask (non-blocking)
allChildrenDone :: Priority -> Pool -> Entity -> IO ()
allChildrenDone priority pool parent =
    do
        alloc <- PoolAlloc.startAlloc priority pool
        allocCompletionMVar <- newEmptyMVar
        modifyEntityState parent $
            \(EntityReallocStarting priority' allocStartedMVar) ->
            ( EntityReallocating (max priority priority') alloc allocCompletionMVar
            , do
                when (priority' > priority) $
                    PoolAlloc.changePriority alloc priority'
                putMVar allocStartedMVar alloc
            )

-- runs under mask (non-blocking)
notifyParent :: Pool -> Entity -> Entity -> IO ()
notifyParent pool child parent =
    modifyEntityState parent $
    \(EntityReleasedToChildren priority children allocStartedMVar) ->
    case List.partition (== child) children of
    ([_], []) ->
        ( EntityReallocStarting priority allocStartedMVar
        , allChildrenDone priority pool parent
        )
    ([_], rest) ->
        ( EntityReleasedToChildren priority rest allocStartedMVar
        , return ()
        )
    _ -> error "Expecting to find child exactly once in parent's list of children"

-- runs under mask (non-blocking)
childDone :: Pool -> Entity -> IO ()
childDone pool child =
    join $ atomicModifyIORef (entityNotifyParents child) $ \oldState ->
    case oldState of
    EntityWillNotify parents ->
        ( EntityAlreadyNotified
        , mapM_ (notifyParent pool child) parents
        )
    EntityAlreadyNotified -> error "Child done twice?!"

-- runs under mask (non-blocking)
linkChild :: Pool -> Entity -> Entity -> IO ()
linkChild pool parent child =
    join $ atomicModifyIORef (entityNotifyParents child) $ \oldState ->
    case oldState of
    EntityWillNotify parents -> (EntityWillNotify (parent:parents), return ())
    EntityAlreadyNotified -> (EntityAlreadyNotified, notifyParent pool child parent)

-- Runs under mask
-- Interruptible, but exception-safe (no effect if async exception)
releaseToChildren :: Priority -> Pool -> Entity -> [Entity] -> IO ()
releaseToChildren priority pool parent children =
    do
        allocStartedMVar <- newEmptyMVar
        let
            loop =
                modifyEntityState parent $
                \state ->
                case state of
                EntityForking {} -> error "withReleased must be invoked in a child context"
                EntityAlloced parId ->
                    ( EntityReleasedToChildren priority children allocStartedMVar
                    , do
                        -- The following don't throw exceptions and are non-blocking (we're not interruptible)
                        mapM_ (linkChild pool parent) children
                        PoolAlloc.release pool parId
                    )
                EntityReallocStarting allocPriority mvar ->
                    ( EntityReallocStarting (max priority allocPriority) mvar
                    , -- blocking readMVar may throw async exception, but
                      -- that is a no-op at this point, so is fine
                      readMVar mvar >> loop
                    )
                EntityReallocating _priority _alloc mvar ->
                    ( state
                    , -- ditto about blocking
                      readMVar mvar >> loop
                    )
                EntityReleasedToChildren {} -> error "entityReleaseCounter should prevent this!"
                EntityFinished -> error "EntityFinished in release to children?!"
        loop

-- Runs under uninterruptibleMask, thus exception-safe
reallocFromChildren :: Priority -> Entity -> IO ()
reallocFromChildren priority parent =
    modifyEntityState parent $ \state ->
    case state of
    EntityReleasedToChildren reallocPriority children allocStartedMVar ->
        ( EntityReleasedToChildren (max priority reallocPriority) children
          allocStartedMVar
        , finishAlloc allocStartedMVar
        )
    EntityReallocStarting reallocPriority allocStartedMVar ->
        ( EntityReallocStarting (max priority reallocPriority) allocStartedMVar
        , finishAlloc allocStartedMVar
        )
    EntityReallocating reallocPriority alloc allocCompletionMVar ->
        ( EntityReallocating (max priority reallocPriority) alloc allocCompletionMVar
        , do
            when (priority > reallocPriority) $
                PoolAlloc.changePriority alloc priority
            PoolAlloc.finish alloc >>= setAlloced
        )
    _ -> error "realloc at invalid state"
    where
        finishAlloc allocStartedMVar =
            readMVar allocStartedMVar
            >>= PoolAlloc.finish
            >>= setAlloced
        setAlloced parId =
            modifyEntityState parent $ \state ->
            case state of
            EntityReallocating _reallocPriority _alloc allocCompleteMVar ->
                (EntityAlloced parId, putMVar allocCompleteMVar parId)
            _ -> error "Entity state changed underneath us?!"

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and realloced once. The realloc will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

withReleased :: Priority -> Pool -> Entity -> [Entity] -> IO r -> IO r
withReleased priority pool parent dupChildren =
    bracket_ maybeReleaseToChildren maybeReallocFromChildren
    where
        maybeReleaseToChildren =
            onReleaseCounter $
            \n -> (n + 1, when (n == 0) $ releaseToChildren priority pool parent children)
        maybeReallocFromChildren =
            onReleaseCounter $
            \n -> (n - 1, when (n == 1) $ reallocFromChildren priority parent)
        onReleaseCounter = join . atomicModifyIORef' (entityReleaseCounter parent)
        children = List.nub dupChildren

-- | Must use the resulting wrapper on the child (which implies
-- masking this call)!
fork :: Pool -> Priority -> IO (Entity, IO a -> IO a)
fork pool priority =
    do
        alloc <- PoolAlloc.startAlloc priority pool
        stateRef <- newIORef $ EntityForking priority alloc
        notifyParentsRef <- newIORef $ EntityWillNotify []
        releaseCounterRef <- newIORef 0
        let entity =
                Entity
                { entityState = stateRef
                , entityReleaseCounter = releaseCounterRef
                , entityNotifyParents = notifyParentsRef
                }
        return (entity, wrapChild pool entity alloc)

-- Must run masked (implied by wrapChild being guaranteed to run)
wrapChild :: Pool -> Entity -> Alloc ParId -> IO a -> IO a
wrapChild pool child alloc =
    bracket_ (beforeChild `onException` cancel) afterChild
    where
        setStateAfterForking newState =
            modifyEntityState_ child $ \state ->
            case state of
                EntityForking _priority _alloc -> newState
                _ -> error "wrapChild called in invalid state"
        cancel = setStateAfterForking EntityFinished >> childDone pool child
        beforeChild = setStateAfterForking . EntityAlloced =<< PoolAlloc.finish alloc
        afterChild =
            -- runs under uninterruptibleMask
            modifyEntityState child $
            \oldState ->
            case oldState of
            EntityAlloced parId ->
                ( EntityFinished
                , do
                    childDone pool child
                    PoolAlloc.release pool parId
                )
            _ -> error "forked child did not return to Alloced state"

-- | Boost the priority of an entity and all of its dependencies to at
-- least the given priority. This assumes dependencies of all entities
-- are always at least as high in their priority as their parent (an
-- invariant that is currently unchecked)
boostPriority :: Priority -> Entity -> IO ()
boostPriority priority entity =
    modifyEntityState entity $
    \state ->
        let doNothing = (state, return ())
            boost oldPriority pair
                | priority > oldPriority = pair
                | otherwise = doNothing
        in
        case state of
        EntityForking forkPriority alloc ->
            boost forkPriority
            ( EntityForking priority alloc
            , PoolAlloc.changePriority alloc priority
            )
        EntityReallocating reallocPriority alloc mvar ->
            boost reallocPriority
            ( EntityReallocating priority alloc mvar
            , PoolAlloc.changePriority alloc priority
            )
        EntityReleasedToChildren reallocPriority children mvar ->
            boost reallocPriority
            ( EntityReleasedToChildren priority children mvar
            , mapM_ (boostPriority priority) children
            )
        EntityReallocStarting reallocPriority mvar ->
            ( EntityReallocStarting (max priority reallocPriority) mvar
            , return ()
            )
        -- Not waiting for children, not allocating, do nothing:
        EntityAlloced _parId -> doNothing
        EntityFinished -> doNothing
