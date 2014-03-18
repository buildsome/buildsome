import Distribution.Simple
import System.Process
import System.Exit
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
      putStrLn "Building fs_override.so"
      exitCode <- system "gcc -o fs_override.so -g -Wall -Wextra -Winit-self -shared -fPIC -D_GNU_SOURCE fs_override.c canonize_path.c -ldl -lbsd"
      case exitCode of
        ExitSuccess -> return (Nothing, [])
        ExitFailure i -> fail $ "fs_override.so build failed with " ++ show i
  }
