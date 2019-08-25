module DzenDhall.Animation.Marquee where

import           DzenDhall.AST
import           DzenDhall.Config
import           DzenDhall.Data
import qualified DzenDhall.Extra as Extra

import           Data.Function (fix)
import           Lens.Micro

run :: Int -> Marquee -> AST -> Int -> AST
run fontWidth settings ast frameCounter =
  let framesPerChar = settings ^. mqFramesPerChar
      desiredWidth  = settings ^. mqWidth
      shouldWrap    = settings ^. mqShouldWrap

      realWidth     = astWidth ast
      difference    = realWidth - desiredWidth in

  if | difference <= 0 && not shouldWrap ->
         -- Pad AST
         let padding = ASTText (Extra.spaces (- difference)) in
           ASTs ast padding

     | otherwise ->
         -- Select a part of an "infinite" version of AST.
         let shifted = snd $
               DzenDhall.AST.split
               -- `framesPerChar` is checked for zero when marshalling.
               ((frameCounter `div` framesPerChar) `mod` realWidth)
               (fix (ASTs ast))
             trimmed = fst $
               DzenDhall.AST.split desiredWidth shifted in
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
    fontWidth - (fontWidth * shift) `div` framesPerChar


-- | Add sub-character shift to the AST and compensate it
addPxShift
  :: Int
  -- ^ Shift in pixels
  -> AST
  -> AST
addPxShift pxShift =
  ASTProp (P (XY (pxShift, 0)))
