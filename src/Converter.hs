module Converter(makeTextDataT) where

import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Definition (Pos,TextType(..),TextData(..))

type InitPos = Pos
type HeightLimit = CInt 

makeTextDataT :: InitPos -> HeightLimit 
                              -> TextType -> T.Text -> [TextData]
makeTextDataT pos hl ttp tx = 
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx) 
   in case ch of
        '\\' -> let (code,rtxs) = getCodeData txs
                in Code code : makeTextDataT pos hl ttp rtxs 
        _ -> if txs==T.empty 
              then [Txt ttp pos ch] 
              else let (hd,tl) = fromMaybe ('0',T.empty) (T.uncons txs)
                    in if hd=='：' 
                        then 
           let (rb,tl2) = getRubi tl 
            in TxtR ttp pos ch rb : makeTextDataT (newPosT pos hl) hl ttp tl2
                        else
               Txt ttp pos ch : makeTextDataT (newPosT pos hl) hl ttp txs 

getCodeData :: T.Text -> (T.Text,T.Text)
getCodeData = T.break (=='\n')

getRubi :: T.Text -> (T.Text,T.Text)
getRubi tx = let (rb,tl) = T.break (=='：') tx
                 tl2
                    | tl==T.empty = T.empty 
                    | T.head tl == '：' = T.tail tl
                    | otherwise = tl
              in (rb,tl2)

newPosT :: Pos -> HeightLimit -> Pos 
newPosT (V2 x y) hl = let ty = y + 1
                          isLimit = ty > hl
                          nx = if isLimit then x - 1 else x 
                          ny = if isLimit then 0 else ty
                       in V2 nx ny
