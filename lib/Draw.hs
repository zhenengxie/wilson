module Draw where

import           Graphics.Gloss                 ( Picture
                                                , translate
                                                , rotate
                                                , polygon
                                                , pictures
                                                )

type Coord = (Int, Int)

drawEdge :: Float -> ((Int, Int), (Int, Int)) -> Picture
drawEdge pad ((i, j), (k, l)) =
  translate (fromIntegral i) (fromIntegral j) $ case (k - i, l - j) of
    (0 , 1 ) -> rect
    (1 , 0 ) -> rotate 90 rect
    (0 , -1) -> rotate 180 rect
    (-1, 0 ) -> rotate 270 rect
    _        -> error "edge should not exist"
 where
  rect = polygon [(-pad, -pad), (pad, -pad), (pad, 1 + pad), (-pad, 1 + pad)]

drawTree :: Float -> [(Coord, Coord)] -> Picture
drawTree pad = pictures . map (drawEdge pad)
