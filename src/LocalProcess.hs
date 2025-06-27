{-# OPTIONS_GHC -Wno-unused-imports #-}
module LocalProcess ( runProcessLocal, runProcessLocal', runProcessLocal'' ) where

import qualified Data.Map as M

-- LocalComputation library files
import ValuationAlgebra
import Collect
import Bayesian
import JoinTree
import ShenoyShafer
import Utils


---- We will need these someday (probably)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP
import Network.Transport (Transport, closeTransport)
import Control.Concurrent.MVar.Strict (newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.DeepSeq (NFData)

import Control.Exception (assert, throw)


-- If we do tests in parallel I'm guessing using the same tcp addr could probably cause errors.
-- Maybe see network-transport-inmemory in this case.
runProcessLocal' :: (NFData a) => Process a -> IO a
runProcessLocal' process = do
    resultPointer <- newEmptyMVar

    _ <- runProcessLocal 8080 $ do
        result <- process
        settingResultIsSuccess <- liftIO $ tryPutMVar resultPointer result
        assert settingResultIsSuccess $ pure ()

    y <- tryTakeMVar resultPointer
    case y of
         Nothing -> error "issue"
         Just x -> pure x

runProcessLocal'' :: Process () -> Transport -> IO ()
runProcessLocal'' = undefined
-- runProcessLocal'' :: Process () -> Transport -> IO ()
-- runProcessLocal'' process = do
--
--     transportOrFail <- createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
--     case transportOrFail of
--          Left e -> error "issue"
--          Right transport -> do
--                 node <- newLocalNode transport initRemoteTable
--                 runProcess node process
--                 node2 <- newLocalNode transport initRemoteTable
--                 runProcess node2 process










    -- result' <- newEmptyMVar
    -- runProcess node $ do
    --   ...
    -- -- must never fail, we prefer exception over blocking handler that holds on to memory
    -- Just (HttpResponse status body) <- tryTakeMVar result'
    -- respond $ responseLBS status [] (BSL.fromStrict body)


-- Creates a new transport and closes it on finish.
-- Likely can be more efficent if the transport is kept
-- around for the next operation, running the process with
-- the same transport instead.
runProcessLocal :: Integer -> Process () -> IO Integer
runProcessLocal x process = do

    transportOrFail <- createTransport (defaultTCPAddr "127.0.0.1" (show x)) defaultTCPParameters
    case transportOrFail of
         Left e -> do
             putStrLn $ "skipping " ++ show x
             runProcessLocal (x+1) process
         Right transport -> do
                node <- newLocalNode transport initRemoteTable
                runProcess node process
                closeTransport transport
                pure (x+1)
