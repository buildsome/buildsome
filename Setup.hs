import Distribution.Simple
import System.Process
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
      putStrLn "Building fs_override.so"
      system "gcc -o fs_override.so -g -Wall -Wextra -Winit-self -shared -fPIC fs_override.c -ldl -lbsd"
      return (Nothing, [])
  }
