{- You can use this file as your `config.dhall`. -}
let prelude = ./prelude/package.dhall

let types = ./types/package.dhall

let utils = ./utils/package.dhall

let Bar = types.Bar

let Carrier = types.Carrier

let Color = types.Color

let Configuration = types.Configuration

let Fade = types.Fade

let Marquee = types.Marquee

let Settings = types.Settings

let Slider = types.Slider

let VerticalDirection = types.VerticalDirection

let mkFade : VerticalDirection → Natural → Natural → Fade = utils.mkFade

let mkSlider : Fade → Fade → Natural → Slider = utils.mkSlider

let defaults = utils.defaults

let bar
	: Bar
	=   λ(Bar : Type)
	  → λ(cr : Carrier Bar)
	  → let text : Text → Bar = cr.text

		let join : List Bar → Bar = cr.join

		let fg : Color → Bar → Bar = cr.fg

		let slider : Slider → List Bar → Bar = cr.slider

		let bashWithBinaries
			: List Text → Natural → Text → Bar
			= utils.mkBashWithBinaries Bar cr

		let separateBy : Bar → List Bar → Bar = utils.mkSeparateBy Bar cr

		let separate : List Bar → Bar = separateBy (text " | ")

		let memoryUsage
			: Bar
			= bashWithBinaries
			  [ "free", "grep", "echo", "awk" ]
			  5000
			  ''
			  TMP=`free -b | grep 'Mem'`
			  TotalMem=`echo "$TMP" | awk '{ print $2; }'`
			  UsedMem=`echo "$TMP" | awk '{ print $3; }'`
			  echo "$((UsedMem * 100 / TotalMem))"
			  ''

		let swapUsage
			: Bar
			= bashWithBinaries
			  [ "free", "grep", "echo", "awk" ]
			  5000
			  ''
			  TMP=`free -b | grep 'Swap'`
			  TotalSwap=`echo "$TMP" | awk '{ print $2; }'`
			  UsedSwap=`echo "$TMP" | awk '{ print $3; }'`
			  echo "$((UsedSwap * 100 / TotalSwap))"
			  ''

		let date : Bar = bashWithBinaries [ "date" ] 1000 "date +'%d.%m.%Y'"

		let time : Bar = bashWithBinaries [ "date" ] 1000 "date +'%H:%M'"

		let accent : Bar → Bar = fg "white"

		let fadeIn : Fade = mkFade VerticalDirection.Up 5 16

		let fadeOut : Fade = mkFade VerticalDirection.Down 5 16

		in    slider
			  (mkSlider fadeIn fadeOut 3000)
			  [ join [ text "Mem: ", accent memoryUsage, text "%" ]
			  , join [ text "Swap: ", accent swapUsage, text "%" ]
			  ]
			: Bar

in    utils.mkConfigs [ { bar = bar, settings = defaults.settings } ]
	: List Configuration
