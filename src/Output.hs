module Output(outInit) where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Console.ANSI (clearScreen,setCursorPosition)
import Control.Concurrent.Timer.Lifted(repeatedTimer)
import Control.Concurrent.Suspend(msDelay)
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad (when)
import Linear.V2 (V2(..))
import Definition
import Input(inputLoop)

outInit :: S.StateT Game IO () 
outInit = do
  liftIO clearScreen
  liftIO $ hSetBuffering stdout NoBuffering
  _ <- repeatedTimer outUpdate (msDelay 100)
  inputLoop
  return ()

outUpdate :: S.StateT Game IO ()
outUpdate = do
  game <- S.get
  let textDatas = txd game
      ntxd = if null textDatas then [] else tail textDatas
      (tp,ps,ch) = if null textDatas then (Nml,V2 0 0,'_') else getTPC (head textDatas)
      ngame = game{txd=ntxd}
  when (not (null textDatas) && ch/='_') $ putText tp ps ch 
  S.put ngame

getTPC :: TextData -> (TextType, Pos, Char)
getTPC (Txt tp ps ch) = (tp,ps,ch)
getTPC (TxtR {}) = (Nml,V2 0 0,'_')
getTPC (Code _) = (Nml,V2 0 0,'_')

putText :: MonadIO m => TextType -> Pos -> Char -> m ()
putText _ (V2 x y) ch = do
  liftIO $ do 
    setCursorPosition (fromIntegral y) (fromIntegral x)  
    putChar ch
