module DzenDhall.Animation.Marquee where

import           DzenDhall.Config
import           DzenDhall.Data
import qualified DzenDhall.Extra as Extra
import           Lens.Micro.Extras

run :: MarqueeSettings -> AST -> Int -> AST
run settings ast frameCounter =
  let desiredWidth = view mqWidth settings
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
               DzenDhall.Data.split (frameCounter `mod` (difference + 1)) ast
             trimmed = fst $
               DzenDhall.Data.split desiredWidth shifted in
           trimmed
