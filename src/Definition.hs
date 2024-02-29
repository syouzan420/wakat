{-# LANGUAGE OverloadedStrings #-}
module Definition where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Foreign.C.Types (CInt)

type Pos = V2 CInt
type Title = T.Text
data TextSection = TS Title T.Text deriving (Eq,Show)
data TextType = Nml | Wst | Img deriving (Eq,Show)
data TextData = Txt TextType Pos Char 
              | TxtR TextType Pos Char T.Text 
              | Code T.Text 
                      deriving (Eq,Show)

data MapCell = Path | Road | Field | Wood | Forest | Wall | Block | Water 
                                                          deriving (Eq,Show,Enum)
type MapWhole = [[MapCell]]


data Shape = Round | Cubic | Flat deriving (Eq,Show)
data Element = A | I | U | E | O deriving (Eq,Show)
type Size = Int
type Hardness = Int
type Temperature = Int
type Power = Int
data Direction = East | EN | North | NW | West | WS | South | SE deriving (Eq,Show)
data Verb = Be | Hit | Throw | Emit | Guard | Use deriving (Eq,Show)
data Mana = Mana T Y deriving (Eq,Show)
data Arg = Arg Direction Mana deriving (Eq,Show)
data T = T{shape :: Shape,
           size :: Size,
           hardness :: Hardness,
           temperature :: Temperature,
           power :: Power,
           element :: Element
          } deriving (Eq,Show)
data Y = Y Verb [Arg] deriving (Eq,Show)

--nme: name , pos: position, hnd: hand (left,right)
data Chra = Chra{nme :: T.Text, pos :: Pos, hnd :: (Maybe Mana,Maybe Mana)} 
                        deriving (Eq,Show)

--txd: text data, txs: text sections, itx: is text showing?
--mpd: map datas, chs: characters(head is the player)
data Game = Game{txd :: ![TextData], txs :: ![TextSection], itx :: !Bool
                ,mpd :: ![MapWhole], chs :: ![Chra] }
                        deriving (Eq,Show)

textFile :: FilePath
textFile = "text/waka"

mapCh :: T.Text
mapCh = "0123456789abcdefghi"

textInitPos :: Pos
textInitPos = V2 45 3 

textIndent :: CInt
textIndent = 3

textHeightLimit :: CInt
textHeightLimit = 25 
