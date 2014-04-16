import Control.Concurrent.Async (asyncOn, wait)

threadHandler n = readFile $ "after_sleep." ++ show n

main = do
  -- To get actual concurrency we need to use different POSIX threads,
  -- because otherwise the user-threads share the same buildsome
  -- client, which will re-serialize them
  a <- asyncOn 0 $ threadHandler 1
  b <- asyncOn 1 $ threadHandler 2
  wait a
  wait b
