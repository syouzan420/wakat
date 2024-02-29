module AppMain(appMain) where

import qualified Control.Monad.State.Strict as S
import Output(outInit)
import Load(makeMapAndText)
import Initialize(newGame)
import Converter (makeTextDataT)
import Definition (textInitPos,textIndent,textHeightLimit,TextType(..),TextSection(..),Game(..))

appMain :: IO ()
appMain = do
  (sections,maps) <- makeMapAndText 0
  let (TS _ tx) = head sections 
      initTextData = makeTextDataT textInitPos textIndent textHeightLimit Nml tx
  _ <- S.runStateT outInit newGame{txd=initTextData,txs=sections,mpd=maps}
  return ()
