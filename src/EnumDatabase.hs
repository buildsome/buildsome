{-# OPTIONS -Wall -O2 #-}
import Control.Monad
import Data.ByteString (empty)
import Lib.Binary (decode)
import Pretty ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Buildsome.Db as Db
import qualified Control.Exception as E
import qualified Database.Sophia as Sophia
import qualified System.Environment as Env

whileM :: Monad m => m Bool -> m () -> m ()
whileM boolAct act = go
  where
    go = do
      p <- boolAct
      when p $ do
        act
        go


main :: IO ()
main = do
  [filePath] <- Env.getArgs
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation filePath
    Sophia.withDb env $ \db ->
      Sophia.withCursor db Sophia.GT empty $ \cursor -> do
        putStrLn "Keys:"
        whileM (Sophia.fetchCursor cursor) $ do
          key <- Sophia.keyAtCursor cursor
          val <- Sophia.valAtCursor cursor
          execLogDoc <-
            E.evaluate (pPrint (decode val :: Db.ExecutionLog))
            `E.catch` \E.SomeException {} -> return "not an execution log"
          putStrLn $ "Key " ++ take 8 (show key) ++ "... = " ++ show execLogDoc
        putStrLn "Done"
