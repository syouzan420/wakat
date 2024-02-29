module Input where

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import System.IO.HiddenChar
import Definition

inputLoop :: S.StateT Game IO ()
inputLoop = do
  game <- S.get
  ch <- liftIO getHiddenChar
  S.put game
  liftIO $ putStrLn [ch]
  unless (ch=='e') inputLoop
  
