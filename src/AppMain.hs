module AppMain(appMain) where

import qualified Control.Monad.State.Strict as S
import Output(outInit)
import Initialize(newGame)

appMain :: IO ()
appMain = do
  putStrLn "Hello world"
  _ <- S.runStateT outInit newGame
  return ()
