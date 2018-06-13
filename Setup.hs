import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.Program.Db

sources :: [FilePath]
sources = ["cbits/fs_override.c", "cbits/canonize_path.c", "cbits/client.c"]

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ flags -> do
      let verbosity = fromFlag $ buildVerbosity flags
      putStrLn "Building cbits/fs_override.so"
      (gcc, _) <- requireProgram verbosity gccProgram defaultProgramDb
      runProgram verbosity gcc . words $ "-o cbits/fs_override.so -g -Wall -Werror -Wextra -Winit-self -shared -fPIC -D_GNU_SOURCE " ++ unwords sources ++ " -ldl"
      pure (Nothing, [])
  }
