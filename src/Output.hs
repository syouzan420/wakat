module Output where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import Control.Concurrent.Timer.Lifted(repeatedTimer)
import Control.Concurrent.Suspend(msDelay)
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (liftIO)
import Definition
import Input(inputLoop)

outInit :: S.StateT Game IO () 
outInit = do
  liftIO $ hSetBuffering stdout NoBuffering
  _ <- repeatedTimer outUpdate (msDelay 100)
  inputLoop
  return ()

outUpdate :: S.StateT Game IO ()
outUpdate = do
  game <- S.get
  liftIO $ putStrLn "hey"
  S.put game

