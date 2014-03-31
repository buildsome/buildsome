import Distribution.Simple
import System.Process
import System.Exit

sources = ["cbits/fs_override.c", "cbits/canonize_path.c"]

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
      putStrLn "Building cbits/fs_override.so"
      exitCode <- system $ "gcc -o cbits/fs_override.so -g -Wall -Werror -Wextra -Winit-self -shared -fPIC -D_GNU_SOURCE " ++ unwords sources ++ " -ldl"
      case exitCode of
        ExitSuccess -> return (Nothing, [])
        ExitFailure i -> fail $ "cbits/fs_override.so build failed with " ++ show i
  }
