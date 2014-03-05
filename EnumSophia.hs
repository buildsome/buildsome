{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Database.Sophia as Sophia
import System.Environment (getArgs)
import qualified Data.ByteString as BS

showBS x = "BS.pack " ++ show (BS.unpack x)

main = do
  [dbDir] <- getArgs
  withEnv $ \env -> do
    openDir env ReadOnly DisallowCreation dbDir
    withDb env $ \db ->
      withCursor db Sophia.GT "" $ \cursor -> do
        rows <- fetchCursorAll cursor
        forM rows $ \(key, val) ->
          putStrLn $ concat [showBS key, " = ", showBS val]
