{-# LANGUAGE OverloadedStrings #-}
module Initialize where

import Linear.V2 (V2(..))
import Definition

newGame :: Game
newGame = Game{txd=[],txs=[],itx=True,mpd=[],chs=[initChra]}

initChra :: Chra
initChra = Chra{nme="player",pos=V2 0 0,hnd=(Nothing,Nothing)}

