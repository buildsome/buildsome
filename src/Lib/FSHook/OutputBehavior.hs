module Lib.FSHook.OutputBehavior
  ( OutputEffect(..)
  , OutputBehavior(..)
  , nonExistingFileChanger
  , existingFileChanger
  , fileChanger
  ) where

import Prelude.Compat

data OutputEffect = OutputEffectNone | OutputEffectChanged
  deriving (Eq, Ord, Show)

data OutputBehavior = OutputBehavior
  { whenFileExists   :: OutputEffect
  , whenFileMissing :: OutputEffect
  } deriving (Eq, Ord, Show)

nonExistingFileChanger :: OutputBehavior
nonExistingFileChanger = OutputBehavior
  { whenFileExists  = OutputEffectNone
  , whenFileMissing = OutputEffectChanged
  }

existingFileChanger :: OutputBehavior
existingFileChanger = OutputBehavior
  { whenFileExists  = OutputEffectChanged
  , whenFileMissing = OutputEffectNone
  }

fileChanger :: OutputBehavior
fileChanger = OutputBehavior
  { whenFileExists  = OutputEffectChanged
  , whenFileMissing = OutputEffectChanged
  }
