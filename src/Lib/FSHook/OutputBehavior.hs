module Lib.FSHook.OutputBehavior
  ( KeepsOldContent(..)
  , OutputEffect(..)
  , OutputBehavior(..)
  , nonExistingFileChanger
  , existingFileChanger
  , fileChanger
  ) where

-- WARNING: Must preserve: KeepsOldContent > KeepsNoOldContent!
data KeepsOldContent = KeepsNoOldContent | KeepsOldContent
  deriving (Eq, Ord, Show)
-- If it keeps old content, then this output file is not safe to
-- delete later

data OutputEffect = OutputEffectNone | OutputEffectChanged KeepsOldContent
  deriving (Eq, Ord, Show)

data OutputBehavior = OutputBehavior
  { whenFileExists   :: OutputEffect
  , whenFileMissing :: OutputEffect
  } deriving (Eq, Ord, Show)

nonExistingFileChanger :: KeepsOldContent -> OutputBehavior
nonExistingFileChanger preservation = OutputBehavior
  { whenFileExists  = OutputEffectNone
  , whenFileMissing = OutputEffectChanged preservation
  }

existingFileChanger :: KeepsOldContent -> OutputBehavior
existingFileChanger preservation = OutputBehavior
  { whenFileExists  = OutputEffectChanged preservation
  , whenFileMissing = OutputEffectNone
  }

fileChanger :: KeepsOldContent -> OutputBehavior
fileChanger preservation = OutputBehavior
  { whenFileExists  = OutputEffectChanged preservation
  , whenFileMissing = OutputEffectChanged preservation
  }
