module Lib.FSHook.OutputBehavior
  ( HasEffect(..)
  , OutputBehavior(..)
  , nonExistingFileChanger
  , existingFileChanger
  , fileChanger
  ) where

data HasEffect = NoEffect | HasEffect

data OutputBehavior = OutputBehavior
  { behaviorWhenFileDoesExist :: HasEffect
  , behaviorWhenFileDoesNotExist :: HasEffect
  }

nonExistingFileChanger :: OutputBehavior
nonExistingFileChanger = OutputBehavior
  { behaviorWhenFileDoesExist = NoEffect
  , behaviorWhenFileDoesNotExist = HasEffect
  }

existingFileChanger :: OutputBehavior
existingFileChanger = OutputBehavior
  { behaviorWhenFileDoesExist = HasEffect
  , behaviorWhenFileDoesNotExist = NoEffect
  }

fileChanger :: OutputBehavior
fileChanger = OutputBehavior
  { behaviorWhenFileDoesExist = HasEffect
  , behaviorWhenFileDoesNotExist = HasEffect
  }
