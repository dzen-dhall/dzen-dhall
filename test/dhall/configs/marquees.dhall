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

let mkMarquee : Natural → Natural → Bool → Marquee = utils.mkMarquee

let defaults = utils.defaults

let bar
	: Bar
	=   λ(Bar : Type)
	  → λ(cr : Carrier Bar)
	  → let text : Text → Bar = cr.text

		let join : List Bar → Bar = cr.join

		let fg : Color → Bar → Bar = cr.fg

		let marquee : Marquee → Bar → Bar = cr.marquee

		let separateBy : Bar → List Bar → Bar = utils.mkSeparateBy Bar cr

		let separate : List Bar → Bar = separateBy (text " | ")

		in    separate
			  [ marquee
				(mkMarquee 5 15 False)
				( text
				  "The most annoying HTML code in the history of HTML codes."
				)
			  , marquee
				(mkMarquee 0 32 True)
				( text
				  "The most annoying HTML code in the history of HTML codes."
				)
			  , marquee (mkMarquee 3 10 True) (text "test...")
			  , marquee (mkMarquee 3 10 False) (text "test...")
			  , marquee
				(mkMarquee 8 15 False)
				( join
				  [ text "The "
				  , fg "white" (text "most ")
				  , text "annoying "
				  , fg "white" (text "HTML ")
				  , text "code "
				  , fg "white" (text "in ")
				  , text "the "
				  , fg "white" (text "history ")
				  , text "of "
				  , fg "white" (text "HTML ")
				  , text "codes. "
				  ]
				)
			  ]
			: Bar

in    utils.mkConfigs
	  [ { bar = bar, settings = defaults.settings ⫽ { fontWidth = 8 } } ]
	: List Configuration
