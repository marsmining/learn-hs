module Stm where
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef

greet :: String -> IO ()
greet name = putStrLn ("Hello " ++ name ++ "!!")

nTimes :: Int -> IO () -> IO ()
nTimes 0 _ = return ()
nTimes n fn = do { fn; nTimes (n - 1) fn }

myref = newIORef 5
printRef :: (Show a) => IO (IORef a) -> IO ()
printRef ioref = do { ref <- ioref;
                      v <- readIORef ref;
                      putStrLn $ "ref is: " ++ (show v) }

type Account = TVar Int

wdl :: Account -> Int -> STM ()
wdl acc n = do { bal <- readTVar acc;
                 writeTVar acc (bal - n) }

dep :: Account -> Int -> STM ()
dep acc n = wdl acc (- n)

transfer :: Account -> Account -> Int -> IO ()
transfer from to n = atomically (do { wdl from n;
                                      dep to n })

pa :: Account -> IO ()
pa acc = do { bal <- readTVarIO acc;
                        putStrLn $ "Account balance: " ++ (show bal) }

sim :: IO ()
sim = do { putStrLn "starting sim..";
           aca <- newTVarIO 50;
           acb <- newTVarIO 100;
           pa aca; pa acb;
           transfer aca acb 3;
           pa aca; pa acb;
           putStrLn "finished" }

main :: IO ()
main = do { tid <- forkIO $ putStrLn "forked!"
          ; putStrLn $ "main: " ++ show tid
          ; nTimes 5 $ putStrLn "woohoo!" }
