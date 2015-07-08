{-# LANGUAGE DeriveDataTypeable #-}
module Lib.AnnotatedException
  ( annotateException
  , AnnotatedException(..)
  , unannotateException
  ) where

import Data.Typeable (Typeable)
import qualified Control.Exception as E

data AnnotatedException = AnnotatedException String E.SomeException deriving (Typeable)

instance Show AnnotatedException where
  show (AnnotatedException str e) = str ++ show e
instance E.Exception AnnotatedException

annotateException :: String -> IO a -> IO a
annotateException str = E.handle wrapper
  where
    wrapper e@E.SomeException {} = E.throwIO (AnnotatedException str e)

unannotateException :: E.SomeException -> E.SomeException
unannotateException e =
    case E.fromException e of
    Just (AnnotatedException _ inner) -> unannotateException inner
    _ -> e
