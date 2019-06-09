module DzenDhall.Animation.Marquee where

import           DzenDhall.Config
import           DzenDhall.Data
import qualified DzenDhall.Extra as Extra
import           Lens.Micro.Extras

run :: Int -> Marquee -> AST -> Int -> AST
run fontWidth settings ast frameCounter =
  let framesPerChar = view mqFramesPerChar settings
      desiredWidth  = view mqWidth settings
      realWidth     = astWidth ast
      difference    = realWidth - desiredWidth in

  if | difference == 0 -> ast
     | difference <  0 ->
       -- Pad AST
       let padding = ASTText (Extra.spaces (- difference)) in
         ASTs ast padding
     | otherwise       ->
         -- Select a part of AST
         let shifted = snd $
               DzenDhall.Data.split ((frameCounter `div` framesPerChar) `mod` (difference + 1)) ast
             trimmed = fst $
               DzenDhall.Data.split desiredWidth shifted in
           if | framesPerChar == 1 -> trimmed
              | otherwise ->
                let pxShift = calculatePxShift frameCounter fontWidth framesPerChar in
                  addPxShift pxShift trimmed

-- | Calculate shift in pixels.
calculatePxShift
  :: Int
  -- ^ Number of current frame
  -> Int
  -- ^ Character width
  -> Int
  -- ^ How many frames per character?
  -> Int
  -- ^ Shift in pixels
calculatePxShift frameCounter fontWidth framesPerChar =
  let shift = frameCounter `mod` framesPerChar in
    fontWidth `div` 2 - ((fontWidth * shift) `div` framesPerChar)

-- | Add sub-character shift to AST and compensate it
addPxShift
  :: Int
  -- ^ Shift in pixels
  -> AST
  -> AST
addPxShift pxShift ast =
  let shifted     = Prop (P (XY (  pxShift, 0))) ast
      compensator = Prop (P (XY (- pxShift, 0))) mempty
  in
    shifted <> compensator
