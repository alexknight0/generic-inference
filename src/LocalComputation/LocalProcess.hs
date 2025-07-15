{-# OPTIONS_GHC -Wno-unused-imports #-}
module LocalComputation.LocalProcess (runProcessLocal) where

import qualified Data.Map                         as M

-- LocalComputation library files
import           LocalComputation.Utils


---- We will need these someday (probably)
import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.MVar.Strict   (newEmptyMVar, tryPutMVar,
                                                   tryTakeMVar)
import           Control.DeepSeq                  (NFData)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Network.Transport                (Transport, closeTransport)
import           Network.Transport.TCP

import           Control.Exception                (assert, throw)
import qualified Control.Exception                as E
import           GHC.IO.Exception                 (IOErrorType (ResourceBusy),
                                                   ioe_type)

maxTcpPortNum :: Integer
maxTcpPortNum = 65535

defaultPort :: Integer
defaultPort = 8080

{- | Runs a process locally and returns the result. -}
runProcessLocal :: (NFData a) => Process a -> IO a
runProcessLocal process = do
    resultPointer <- newEmptyMVar

    _ <- runProcessLocal' defaultPort $ do
        result <- process
        settingResultIsSuccess <- liftIO $ tryPutMVar resultPointer result
        assert settingResultIsSuccess $ pure ()

    y <- tryTakeMVar resultPointer
    case y of
         Nothing -> error "issue"
         Just x  -> pure x




{- | Runs a process on a local TCP address, returning the port number it is running on.

As this function creates a new transport and closes it on finish, may be inefficent to
call this many times instead of reusing the same transport.
-}
runProcessLocal' :: Integer -> Process () -> IO Integer
runProcessLocal' x process = do

    transportOrFail <- createTransport (defaultTCPAddr "127.0.0.1" (show x)) defaultTCPParameters
    case transportOrFail of
         -- Try a new port if failed because port was in use.
         Left e -> do
             case ioe_type e of
                  ResourceBusy -> assert (x < maxTcpPortNum) $ runProcessLocal' (x+1) process
                  _ -> throw e

         -- Run process with transport, cleaning up even if an exception occurs.
         Right transport -> do
                E.bracket
                    (pure ())
                    (\_ -> closeTransport transport)
                    (\_ -> do
                        node <- newLocalNode transport initRemoteTable
                        runProcess node process)
                pure x

