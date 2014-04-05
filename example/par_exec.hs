import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM)

main = do
  res <- replicateM 10 $ async $ threadDelay 100000 >> readFile "out/after_sleep"
  mapM_ wait res
