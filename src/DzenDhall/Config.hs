{-# LANGUAGE TemplateHaskell #-}
-- | Data types for marshalling dhall configs into Haskell.
module DzenDhall.Config where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.Text (Text)
import           Dhall
import           Lens.Micro.TH
import           Lens.Micro (Lens', _1)

import           DzenDhall.Extra


type AutomatonState = Text

stateDecoder :: Decoder AutomatonState
stateDecoder = union $ constructor "State" strictText

type AutomatonAddress = Text

automatonAddressDecoder :: Decoder AutomatonAddress
automatonAddressDecoder = union $ constructor "Address" strictText

type Scope            = Text
type VariableName     = Text
type Value            = Text
type ImageContents    = Text
type ImageId          = Text

data Marquee
  = Marquee
  { _mqFramesPerChar :: Int
  , _mqWidth         :: Int
  , _mqShouldWrap      :: Bool
  }
  deriving (Show, Eq, Generic)

makeLenses ''Marquee

marqueeDecoder :: Decoder Marquee
marqueeDecoder = record $
  Marquee <$> field "framesPerCharacter" (positive    . fromIntegral <$> natural)
          <*> field "width"              (nonNegative . fromIntegral <$> natural)
          <*> field "shouldWrap"         bool

data Direction
  = DLeft | DRight
  deriving (Show, Eq, Generic)

directionDecoder :: Decoder Direction
directionDecoder = union
  $  (DLeft  <$ constructor "Left"   unit)
  <> (DRight <$ constructor "Right" unit)

data VerticalDirection
  = VUp | VDown
  deriving (Show, Eq, Generic)

verticalDirectionDecoder :: Decoder VerticalDirection
verticalDirectionDecoder = union
  $  (VUp   <$ constructor "Up"   unit)
  <> (VDown <$ constructor "Down" unit)

data Assertion
  = BinaryInPath Text
  | SuccessfulExit Text
  deriving (Show, Eq, Generic)

assertionDecoder :: Decoder Assertion
assertionDecoder = union
  $  (BinaryInPath    <$> constructor "BinaryInPath" strictText)
  <> (SuccessfulExit  <$> constructor "SuccessfulExit"  strictText)

data Check
  = Check { _chMessage :: Text
          , _chAssertion :: Assertion
          }
  deriving (Show, Eq, Generic)

makeLenses ''Check

checkDecoder :: Decoder Check
checkDecoder = record $
  Check <$> field "message"   strictText
        <*> field "assertion" assertionDecoder

data Button
  = MouseLeft
  | MouseMiddle
  | MouseRight
  | MouseScrollUp
  | MouseScrollDown
  | MouseScrollLeft
  | MouseScrollRight
  deriving (Show, Eq, Ord, Generic)

instance Hashable Button

buttonDecoder :: Decoder Button
buttonDecoder = union
  $  (MouseLeft        <$ constructor "Left"        unit)
  <> (MouseMiddle      <$ constructor "Middle"      unit)
  <> (MouseRight       <$ constructor "Right"       unit)
  <> (MouseScrollUp    <$ constructor "ScrollUp"    unit)
  <> (MouseScrollDown  <$ constructor "ScrollDown"  unit)
  <> (MouseScrollLeft  <$ constructor "ScrollLeft"  unit)
  <> (MouseScrollRight <$ constructor "ScrollRight" unit)

newtype Event
  = Event Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable Event

eventDecoder :: Decoder Event
eventDecoder = union $ (Event <$> constructor "Event" strictText)

data Fade
  = Fade
  { _fadeDirection   :: VerticalDirection
  , _fadeFrameCount  :: Int
  , _fadePixelHeight :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Fade

fadeDecoder :: Decoder Fade
fadeDecoder = record $
  Fade <$> field "direction"  verticalDirectionDecoder
       <*> field "frameCount" (fromIntegral <$> natural)
       <*> field "height"     (fromIntegral <$> natural)

data Slider
  = Slider
  { _fadeIn      :: Fade
  , _fadeOut     :: Fade
  , _sliderDelay :: Int

  }
  deriving (Show, Eq, Generic)

makeLenses ''Slider

sliderDecoder :: Decoder Slider
sliderDecoder = record $
  Slider <$> field "fadeIn"  fadeDecoder
         <*> field "fadeOut" fadeDecoder
         <*> field "delay"   (fromIntegral <$> natural)

data Hook
  = Hook
  { _hookCommand          :: [Text]
  , _hookInput            :: Text
  }
  deriving (Show, Eq, Generic)

makeLenses ''Hook

hookDecoder :: Decoder Hook
hookDecoder = record $
  Hook <$> field "command"          (list strictText)
       <*> field "input"            strictText

newtype StateTransitionTable
  = STT { unSTT :: H.HashMap (Scope, Event, AutomatonState) (AutomatonState, [Hook])
        }
  deriving (Show, Eq, Generic)

stateTransitionTableDecoder :: Decoder StateTransitionTable
stateTransitionTableDecoder = STT . H.fromList . concatMap collect <$> list
  ( record
    ( pack5 <$> field "events" (list eventDecoder)
            <*> field "from"   (list stateDecoder)
            <*> field "to"     stateDecoder
            <*> field "hooks"  (list hookDecoder)
    )
  )
  where
    pack5 events froms to hooks = (events, froms, to, hooks)

    collect (events, froms, to, hooks) =

      [ (("", event, from), (to, hooks))
      --        ^ scope is left uninitialized. It will be added later
      | event <- events
      , from <- froms
      ]

_scope :: Lens' (Scope, Event, AutomatonState) Scope
_scope = _1

newtype Color = Color Text
  deriving (Show, Eq, Generic)

colorDecoder :: Decoder Color
colorDecoder = Color <$> strictText

data AbsolutePosition
  = AbsolutePosition { _apX :: Int, _apY :: Int }
  deriving (Show, Eq, Generic)

makeLenses ''AbsolutePosition

absolutePositionDecoder :: Decoder AbsolutePosition
absolutePositionDecoder = record $
  AbsolutePosition <$> field "x" (fromIntegral <$> integer)
                   <*> field "y" (fromIntegral <$> integer)


{- | Specify position that will be passed to @^p()@. -}
data Position =
  -- | @^p(+-X;+-Y)@      - move X pixels to the right or left and Y pixels up or down of the current
  --                      position (on the X and Y axis).
  XY (Int, Int) |
  -- | @^p()@             - Reset the Y position to its default.
  P_RESET_Y |
  -- | @_LOCK_X@          - Lock the current X position, useful if you want to align things vertically
  P_LOCK_X |
  -- | @_UNLOCK_X@        - Unlock the X position
  P_UNLOCK_X |
  -- | @_LEFT@            - Move current x-position to the left edge
  P_LEFT |
  -- | @_RIGHT@           - Move current x-position to the right edge
  P_RIGHT |
  -- | @_TOP@             - Move current y-position to the top edge
  P_TOP |
  -- | @_CENTER@          - Move current x-position to the center of the window
  P_CENTER |
  -- | @_BOTTOM@          - Move current y-position to the bottom edge
  P_BOTTOM
  deriving (Show, Eq, Generic)

positionDecoder :: Decoder Position
positionDecoder = union
  $  (XY           <$> constructor "XY"        xy)
  <> (P_RESET_Y    <$  constructor "_RESET_Y"  unit)
  <> (P_LOCK_X     <$  constructor "_LOCK_X"   unit)
  <> (P_UNLOCK_X   <$  constructor "_UNLOCK_X" unit)
  <> (P_LEFT       <$  constructor "_LEFT"     unit)
  <> (P_RIGHT      <$  constructor "_RIGHT"    unit)
  <> (P_TOP        <$  constructor "_TOP"      unit)
  <> (P_CENTER     <$  constructor "_CENTER"   unit)
  <> (P_BOTTOM     <$  constructor "_BOTTOM"   unit)
  where
    xy = record ((,) <$> (fromIntegral <$> field "x" integer)
                     <*> (fromIntegral <$> field "y" integer))
data ClickableArea
  = ClickableArea { _caButton :: Button
                  , _caCommand :: Text
                  }
  deriving (Show, Eq, Generic)

makeLenses ''ClickableArea

clickableAreaDecoder :: Decoder ClickableArea
clickableAreaDecoder = record $
  ClickableArea <$> field "button"  buttonDecoder
                <*> field "command" strictText

data Padding
  = PLeft
  | PRight
  | PSides
  deriving (Show, Eq, Generic)

paddingDecoder :: Decoder Padding
paddingDecoder = union
  $  (PLeft  <$ constructor "Left"  unit)
  <> (PRight <$ constructor "Right" unit)
  <> (PSides <$ constructor "Sides" unit)

data OpeningTag
  = OMarquee     Marquee
  | OSlider      Slider
  | OFG          Color
  | OBG          Color
  | OP           Position
  | OPA          AbsolutePosition
  | OCA          ClickableArea
  | OIB
  | OPadding     Int  Padding
  | OTrim        Int  Direction
  | OAutomaton   AutomatonAddress StateTransitionTable
  | OStateMapKey Text
  | OScope
  deriving (Show, Eq, Generic)

openingTagDecoder :: Decoder OpeningTag
openingTagDecoder = union
  $  (OMarquee     <$> constructor "Marquee"     marqueeDecoder)
  <> (OSlider      <$> constructor "Slider"      sliderDecoder)
  <> (OFG          <$> constructor "FG"          colorDecoder)
  <> (OBG          <$> constructor "BG"          colorDecoder)
  <> (OP           <$> constructor "P"           positionDecoder)
  <> (OPA          <$> constructor "PA"          absolutePositionDecoder)
  <> (OCA          <$> constructor "CA"          clickableAreaDecoder)
  <> (OIB          <$  constructor "IB"          unit)

  <> (uncurry OPadding <$> constructor "Padding"
       ( record $ (,) <$> field "width"   (fromIntegral <$> natural)
                      <*> field "padding" paddingDecoder
       )
     )

  <> (uncurry OTrim <$> constructor "Trim"
       ( record $ (,) <$> field "width"     (fromIntegral <$> natural)
                      <*> field "direction" directionDecoder
       )
     )

  <> (uncurry OAutomaton <$> constructor "Automaton"
       ( record $ (,) <$> field "address"  automatonAddressDecoder
                      <*> field "stt"      stateTransitionTableDecoder
       )
     )

  <> (OStateMapKey <$> constructor "StateMapKey" stateDecoder)
  <> (OScope       <$  constructor "Scope"       unit)

data BarSettings
  = BarSettings
  { _bsMonitor :: Int
  -- ^ Xinerama monitor number
  , _bsExtraArgs     :: [String]
  -- ^ Extra args to pass to dzen binary
  , _bsUpdateInterval :: Int
  -- ^ In microseconds
  , _bsFont           :: Maybe String
  -- ^ Font in XLFD format
  , _bsFontWidth      :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''BarSettings

barSettingsDecoder :: Decoder BarSettings
barSettingsDecoder = record $
  BarSettings <$> field "monitor"        (fromIntegral <$> natural)
              <*> field "extraArgs"      (list string)
              <*> field "updateInterval" ((* 1000) . fromIntegral <$> natural)
              <*> field "font"           (Dhall.maybe string)
              <*> field "fontWidth"      (fromIntegral <$> natural)


data ShapeSize
  = ShapeSize { _shapeSizeW :: Int, _shapeSizeH :: Int }
  deriving (Show, Eq, Generic)

makeLenses ''ShapeSize

shapeSizeDecoder :: Decoder ShapeSize
shapeSizeDecoder = record $
  ShapeSize <$> field "w" (fromIntegral <$> natural)
            <*> field "h" (fromIntegral <$> natural)

data Variable
  = Variable { _varName  :: Text
             , _varValue :: Text
             }
  deriving (Show, Eq, Generic)

makeLenses ''Variable

variableDecoder :: Decoder Variable
variableDecoder = record $
  Variable <$> field "name"  strictText
           <*> field "value" strictText

data Token
  = TokOpen OpeningTag
  | TokClose
  | TokSeparator
  | TokTxt Text
  | TokSource Source
  | TokMarkup Text
  | TokI Text
  | TokR ShapeSize
  | TokRO ShapeSize
  | TokC Int
  | TokCO Int
  | TokCheck Check
  | TokDefine Variable
  deriving (Show, Eq, Generic)

tokenDecoder :: Decoder Token
tokenDecoder = union
  $  (TokOpen      <$> constructor "Open"      openingTagDecoder)
  <> (TokClose     <$  constructor "Close"     unit)
  <> (TokSeparator <$  constructor "Separator" unit)
  <> (TokTxt       <$> constructor "Txt"       strictText)
  <> (TokSource    <$> constructor "Source"    sourceSettingsDecoder)
  <> (TokMarkup    <$> constructor "Markup"    strictText)
  <> (TokI         <$> constructor "I"         strictText)
  <> (TokR         <$> constructor "R"         shapeSizeDecoder)
  <> (TokRO        <$> constructor "RO"        shapeSizeDecoder)
  <> (TokC         <$> constructor "C"         (fromIntegral <$> natural))
  <> (TokCO        <$> constructor "CO"        (fromIntegral <$> natural))
  <> (TokCheck     <$> constructor "Check"     checkDecoder)
  <> (TokDefine    <$> constructor "Define"    variableDecoder)

stateMapDecoder :: Decoder (H.HashMap Text [Token])
stateMapDecoder = H.fromList <$>
  list (record $
         (,) <$> field "state" strictText
             <*> field "bar"   (list tokenDecoder))

data Source
  = Source
  { updateInterval :: Maybe Int
  -- ^ In microseconds
  , command        :: [String]
  , input          :: Text
  , escape         :: Bool
  } deriving (Show, Eq, Generic)

instance Hashable Source

sourceSettingsDecoder :: Decoder Source
sourceSettingsDecoder = record $
  Source <$> field "updateInterval" (Dhall.maybe $ (* 1000) . fromIntegral <$> natural)
         <*> field "command"        (list string)
         <*> field "input"          strictText
         <*> field "escape"         bool

data Configuration = Configuration
  { _cfgBarTokens   :: [Token]
  , _cfgBarSettings :: BarSettings
  }
  deriving (Show, Eq, Generic)

makeLenses ''Configuration

configurationDecoder :: Decoder Configuration
configurationDecoder = record $
  Configuration <$> field "bar"      (list tokenDecoder)
                <*> field "settings" barSettingsDecoder

data PluginMeta = PluginMeta
  { _pmName             :: Text
  , _pmAuthor           :: Text
  , _pmEmail            :: Maybe Text
  , _pmHomePage         :: Maybe Text
  , _pmUpstream         :: Maybe Text
  , _pmDescription      :: Text
  , _pmUsage            :: Text
  , _pmApiVersion       :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''PluginMeta

pluginMetaDecoder :: Decoder PluginMeta
pluginMetaDecoder = record $
  PluginMeta <$> field "name"             strictText
             <*> field "author"           strictText
             <*> field "email"            (Dhall.maybe strictText)
             <*> field "homepage"         (Dhall.maybe strictText)
             <*> field "upstream"         (Dhall.maybe strictText)
             <*> field "description"      strictText
             <*> field "usage"            strictText
             <*> field "apiVersion"       (fromIntegral <$> natural)
