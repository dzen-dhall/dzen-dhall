{-# LANGUAGE TemplateHaskell #-}
-- | Data types for marshalling dhall configs into Haskell.
module DzenDhall.Config where

import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.Text (Text)
import           Dhall
import           Lens.Micro.TH

import           DzenDhall.Extra

data Marquee
  = Marquee
  { _mqFramesPerChar :: Int
  , _mqWidth         :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Marquee

marqueeType :: Type Marquee
marqueeType = record $
  Marquee <$> field "framesPerCharacter" (positive    . fromIntegral <$> natural)
          <*> field "width"              (nonNegative . fromIntegral <$> natural)

data VDirection
  = VUp | VDown
  deriving (Show, Eq, Generic)

directionType :: Type VDirection
directionType = union
  $  (VUp   <$ constructor "Up"   unit)
  <> (VDown <$ constructor "Down" unit)

data MouseButton
  = MouseLeft
  | MouseMiddle
  | MouseRight
  | MouseScrollUp
  | MouseScrollDown
  | MouseScrollLeft
  | MouseScrollRight
  deriving (Show, Eq, Ord, Generic)

instance Hashable MouseButton

mouseButtonType :: Type MouseButton
mouseButtonType = union
  $  (MouseLeft        <$ constructor "Left"        unit)
  <> (MouseMiddle      <$ constructor "Middle"      unit)
  <> (MouseRight       <$ constructor "Right"       unit)
  <> (MouseScrollUp    <$ constructor "ScrollUp"    unit)
  <> (MouseScrollDown  <$ constructor "ScrollDown"  unit)
  <> (MouseScrollLeft  <$ constructor "ScrollLeft"  unit)
  <> (MouseScrollRight <$ constructor "ScrollRight" unit)

data Event
  = MouseEvent MouseButton
  | CustomEvent Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable Event

eventType :: Type Event
eventType = union
  $  (MouseEvent  <$> constructor "Mouse"  mouseButtonType)
  <> (CustomEvent <$> constructor "Custom" strictText)

data Fade
  = Fade
  { _fadeDirection   :: VDirection
  , _fadeFrameCount  :: Int
  , _fadePixelHeight :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Fade

fadeType :: Type Fade
fadeType = record $
  Fade <$> field "direction"  directionType
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

sliderType :: Type Slider
sliderType = record $
  Slider <$> field "fadeIn"  fadeType
         <*> field "fadeOut" fadeType
         <*> field "delay"   (fromIntegral <$> natural)

data Hook
  = Hook
  { _hookCommand           :: [Text]
  , _hookStdIn             :: Maybe Text
  , _hookAllowedExitCodes :: Maybe [Int]
  }
  deriving (Show, Eq, Generic)

makeLenses ''Hook

hookType :: Type Hook
hookType = record $
  Hook <$> field "command"          (list strictText)
       <*> field "stdin"            (Dhall.maybe strictText)
       <*> field "allowedExitCodes" (Dhall.maybe (list (fromIntegral <$> natural)))

newtype StateTransitionTable = STT { unSTT :: H.HashMap (Text, Event, Text) Text }
  deriving (Show, Eq, Generic)

stateTransitionTableType :: Type StateTransitionTable
stateTransitionTableType = STT . H.fromList . concatMap collect <$> list
  ( record
    ( pack4 <$> field "slots"  (list strictText)
            <*> field "events" (list eventType)
            <*> field "from"   (list strictText)
            <*> field "to"     strictText
            <*> field "hooks"  (list hookType)
    )
  )
  where
    pack4 slots events froms to hooks = (slots, events, froms, to)

    collect (slots, events, froms, to) =

      [ ((slot, event, from), to)
      | slot <- slots
      , event <- events
      , from <- froms ]

data Color
  = ColorHex Text
  | ColorName Text
  deriving (Show, Eq, Generic)

colorType :: Type Color
colorType = union
  $  (ColorHex  <$> constructor "hex" strictText)
  <> (ColorName <$> constructor "name" strictText)

data AbsolutePosition
  = AbsolutePosition { x :: Int, y :: Int }
  deriving (Show, Eq, Generic)

absolutePositionType :: Type AbsolutePosition
absolutePositionType = record $
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

positionType :: Type Position
positionType = union
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
  = ClickableArea { _caButton :: MouseButton
                  , _caCommand :: Text
                  }
  deriving (Show, Eq, Generic)

makeLenses ''ClickableArea

clickableAreaType :: Type ClickableArea
clickableAreaType = record $
  ClickableArea <$> field "button"  mouseButtonType
                <*> field "command" strictText

data OpeningTag
  = OMarquee     Marquee
  | OSlider      Slider
  | OFG          Color
  | OBG          Color
  | OP           Position
  | OPA          AbsolutePosition
  | OCA          ClickableArea
  | OIB
  | OAutomaton   StateTransitionTable
  | OStateMapKey Text
  | OListener    Text
  deriving (Show, Eq, Generic)

openingTagType :: Type OpeningTag
openingTagType = union
  $  (OMarquee     <$> constructor "Marquee"     marqueeType)
  <> (OSlider      <$> constructor "Slider"      sliderType)
  <> (OFG          <$> constructor "FG"          colorType)
  <> (OBG          <$> constructor "BG"          colorType)
  <> (OP           <$> constructor "P"           positionType)
  <> (OPA          <$> constructor "PA"          absolutePositionType)
  <> (OCA          <$> constructor "CA"          clickableAreaType)
  <> (OIB          <$  constructor "IB"          unit)
  <> (OAutomaton   <$> constructor "Automaton"   stateTransitionTableType)
  <> (OStateMapKey <$> constructor "StateMapKey" strictText)
  <> (OListener    <$> constructor "Listener"    strictText)

data BarSettings
  = BarSettings
  { _bsMonitor :: Int
  -- ^ Xinerama monitor number
  , _bsExtraFlags     :: [String]
  -- ^ Extra flags to pass to dzen binary
  , _bsUpdateInterval :: Int
  -- ^ In microseconds
  , _bsFont           :: Maybe String
  -- ^ Font in XLFD format
  , _bsFontWidth      :: Maybe Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''BarSettings

barSettingsType :: Type BarSettings
barSettingsType = record $
  BarSettings <$> field "monitor"        (fromIntegral <$> natural)
              <*> field "extraFlags"     (list string)
              <*> field "updateInterval" ((* 1000) . fromIntegral <$> natural)
              <*> field "font"           (Dhall.maybe string)
              <*> field "fontWidth"      (fmap fromIntegral <$> Dhall.maybe natural)


data Image
  = IRelative Text
  | IAbsolute Text
  deriving (Show, Eq, Generic)

imageType :: Type Image
imageType = union
  $  (IRelative <$> constructor "relative" strictText)
  <> (IAbsolute <$> constructor "absolute" strictText)

data ShapeSize
  = ShapeSize { _shapeSizeW :: Integer, _shapeSizeH :: Integer }
  deriving (Show, Eq, Generic)

makeLenses ''ShapeSize

shapeSizeType :: Type ShapeSize
shapeSizeType = record $
  ShapeSize <$> field "w" (fromIntegral <$> natural)
            <*> field "h" (fromIntegral <$> natural)

data Token
  = TokOpen OpeningTag
  | TokRaw Text
  | TokSource Source
  | TokTxt Text
  | TokSeparator
  | TokI Image
  | TokR ShapeSize
  | TokRO ShapeSize
  | TokC Int
  | TokCO Int
  | TokClose
  deriving (Show, Eq, Generic)

tokenType :: Type Token
tokenType = union
  $  (TokOpen      <$> constructor "Open"      openingTagType)
  <> (TokRaw       <$> constructor "Raw"       strictText)
  <> (TokSource    <$> constructor "Source"    sourceSettingsType)
  <> (TokTxt       <$> constructor "Txt"       strictText)
  <> (TokSeparator <$  constructor "Separator" unit)
  <> (TokI         <$> constructor "I"         imageType)
  <> (TokR         <$> constructor "R"         shapeSizeType)
  <> (TokRO        <$> constructor "RO"        shapeSizeType)
  <> (TokC         <$> constructor "C"         (fromIntegral <$> natural))
  <> (TokCO        <$> constructor "CO"        (fromIntegral <$> natural))
  <> (TokClose     <$  constructor "Close"     unit)

stateMapType :: Type (H.HashMap Text [Token])
stateMapType = H.fromList <$>
  list (record $
         (,) <$> field "state" strictText
             <*> field "bar"   (list tokenType))

data EscapeMode = EscapeMode
  { joinLines    :: Bool
  , escapeMarkup :: Bool
  }
  deriving (Show, Eq, Generic)

escapeModeType :: Type EscapeMode
escapeModeType = record $
  EscapeMode <$> field "joinLines"    bool
             <*> field "escapeMarkup" bool

data Source
  = Source
  { updateInterval :: Maybe Int
  -- ^ In microseconds
  , command        :: [String]
  , stdin          :: Maybe Text
  , escapeMode     :: EscapeMode
  } deriving (Show, Eq, Generic)

sourceSettingsType :: Type Source
sourceSettingsType = record $
  Source <$> field "updateInterval" (Dhall.maybe $ (* 1000) . fromIntegral <$> natural)
         <*> field "command"        (list string)
         <*> field "stdin"          (Dhall.maybe strictText)
         <*> field "escapeMode"     escapeModeType

data Configuration = Configuration
  { _cfgBarTokens   :: [Token]
  , _cfgBarSettings :: BarSettings
  }
  deriving (Show, Eq, Generic)

makeLenses ''Configuration

configurationType :: Type Configuration
configurationType = record $
  Configuration <$> field "bar"      (list tokenType)
                <*> field "settings" barSettingsType

data PluginMeta = PluginMeta
  { _pmName             :: Text
  , _pmAuthor           :: Text
  , _pmEmail            :: Maybe Text
  , _pmHomePage         :: Maybe Text
  , _pmUpstreamURL      :: Maybe Text
  , _pmDescription      :: Text
  , _pmUsage            :: Text
  , _pmRequiredBinaries :: [Text]
  , _pmApiVersion       :: Int
  }
  deriving (Show, Eq)

makeLenses ''PluginMeta

pluginMetaType :: Type PluginMeta
pluginMetaType = record $
  PluginMeta <$> field "name"             strictText
             <*> field "author"           strictText
             <*> field "email"            (Dhall.maybe strictText)
             <*> field "homepage"         (Dhall.maybe strictText)
             <*> field "upstreamURL"      (Dhall.maybe strictText)
             <*> field "description"      strictText
             <*> field "usage"            strictText
             <*> field "requiredBinaries" (list strictText)
             <*> field "apiVersion"       (fromIntegral <$> natural)
