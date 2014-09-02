import Control.Concurrent.STM

doSomethingBad :: TVar Int -> IO ()
doSomethingBad v =
    do x <- readTVar v
       writeTVar v (x + 1)
