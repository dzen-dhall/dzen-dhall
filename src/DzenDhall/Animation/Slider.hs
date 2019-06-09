module DzenDhall.Animation.Slider where

import DzenDhall.Config
import DzenDhall.Data
import DzenDhall.Extra
import Lens.Micro

-- | There are three stages of the animation in a cycle:
--
-- 1. Fading in
-- 2. Delay
-- 3. Fading out
run :: Slider -> Int -> [AST] -> AST
run _      _     []   = mempty
-- Short-circuit if `fadeFrameCount`s are all set to zero.
run slider frame asts
  | slider ^. fadeIn  . fadeFrameCount == 0 &&
    slider ^. fadeOut . fadeFrameCount == 0 =
      let astIx       = (frame `div` positive (slider ^. sliderDelay)) `mod` length asts
          ast         = asts !! astIx
      in ast
run slider frame asts =
  -- Calculate total frame counts for each 3 stages:
  let inFrameCount    = positive $ slider ^. fadeIn  . fadeFrameCount
      delayFrameCount =            slider ^. sliderDelay
      outFrameCount   = positive $ slider ^. fadeOut . fadeFrameCount

      -- Find how many frames total in a cycle
      totalFrames     = positive $
        inFrameCount + delayFrameCount + outFrameCount

      -- Find currenlty selected AST index
      astIx           = (frame `div` totalFrames) `mod` length asts

      -- Select visible AST
      ast             = asts !! astIx

      -- Find in which frame we are, starting from the beginning of the
      -- animation cycle.
      frameNumber     = frame `mod` totalFrames

      -- Calculate frame number in a cycle from which stage 3 starts
      outStartFrame   = inFrameCount + delayFrameCount

      -- Find out, on which of the three stages we are:
      --
      -- For 1st & 3rd stages, calculate frame number relative to the first
      -- frame of each stage.
      yShift =
        if | frameNumber <= inFrameCount
             -- For the first stage, `relativeFrameNumber` is just `frameNumber` -
             -- - we are starting from 0.
             -> renderFade (slider ^.fadeIn) frameNumber inFrameCount
           | frameNumber < outStartFrame
             -> 0
           | otherwise
             -- For the third stage, `relativeFrameNumber` equals to how many frames
             -- are remaining
             -> renderFade (slider ^. fadeOut) (totalFrames - frameNumber) outFrameCount
  in
    Prop (P (XY (0, yShift))) ast


renderFade
  :: Fade
  -> Int
  -- ^ Number of the current frame
  -> Int
  -- ^ Total frame count in a stage
  -> Int
renderFade fade relativeFrameNumber frameCount =
  let height = fade ^. fadePixelHeight
      dk = direction (fade ^. fadeDirection)
  in
    dk * (height - height * relativeFrameNumber `div` frameCount)


direction :: VDirection -> Int
direction VUp = -1
direction _   = 1
