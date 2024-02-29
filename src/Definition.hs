module Definition where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Foreign.C.Types (CInt)

type Pos = V2 CInt
data TextType = Nml | Wst | Img deriving (Eq,Show)
data TextData = Txt TextType Pos Char 
              | TxtR TextType Pos Char T.Text 
              | Code T.Text 
                      deriving (Eq,Show)

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

data Game = Game{txd :: [TextData]}
