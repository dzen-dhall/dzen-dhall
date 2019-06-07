module DzenDhall.Animation.Marquee where

import           DzenDhall.Config
import           DzenDhall.Data
import qualified DzenDhall.Extra as Extra
import           Lens.Micro.Extras

run :: Int -> MarqueeSettings -> AST -> Int -> AST
run fontWidth settings ast frameCounter =
  let fpc          = view mqFramesPerChar settings
      desiredWidth = view mqWidth settings
      realWidth    = astWidth ast
      difference   = realWidth - desiredWidth in

  if | difference == 0 -> ast
     | difference <  0 ->
       -- Pad AST
       let padding = ASTText (Extra.spaces (- difference)) in
         ASTs ast padding
     | otherwise       ->
         -- Select a part of AST
         let shifted = snd $
               DzenDhall.Data.split ((frameCounter `div` fpc) `mod` (difference + 1)) ast
             trimmed = fst $
               DzenDhall.Data.split desiredWidth shifted in
           if | fpc == 1  -> trimmed
              | otherwise -> addShift frameCounter fontWidth fpc trimmed

addShift :: Int -> Int -> Int -> AST -> AST
addShift frameCounter fontWidth fpc ast =
  let shift = frameCounter `mod` fpc
      pxShift = fontWidth `div` 2 - ((fontWidth * shift) `div` fpc) in
    Prop (P (XY (pxShift, 0))) ast
