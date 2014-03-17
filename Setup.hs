import Distribution.Simple
import System.Process
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
      putStrLn "Building fs_override.so"
      system "gcc -o fs_override.so -g -Wall -Wextra -Winit-self -shared -fPIC -D_GNU_SOURCE fs_override.c canonize_path.c -ldl -lbsd"
      return (Nothing, [])
  }
