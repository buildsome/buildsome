module Lib.FSHook.AccessType
  ( AccessType(..)
  ) where

data AccessType
  = AccessTypeModeOnly -- access, readlink.  Depend on its existence/permission-modes only. If directory, does not depend on file listing
  | AccessTypeStat -- stat: Depend on some rough attributes of the content
  | AccessTypeFull -- open, opendir, etc.  Depend on the content, and if directory, on the file listing
  deriving (Eq, Ord, Show)
