{-# LANGUAGE DeriveDataTypeable #-}
module Lib.AnnotatedException (annotateException, AnnotatedException(..)) where

import Data.Typeable (Typeable)
import qualified Control.Exception as E

data AnnotatedException = AnnotatedException String E.SomeException deriving (Typeable)

instance Show AnnotatedException where
  show (AnnotatedException str e) = str ++ ": " ++ show e
instance E.Exception AnnotatedException

annotateException :: String -> IO a -> IO a
annotateException str = (`E.catch` wrapper)
  where
    wrapper e@E.SomeException {} = E.throwIO (AnnotatedException str e)

