let Configuration = ./src/Configuration.dhall

let Bar : Type = ./src/Bar.dhall

let BarSpec : Type = ./src/BarSpec.dhall

let makeBar : Bar → BarSpec = ./src/makeBar.dhall

let BarSettings : Type = ./src/BarSettings.dhall

let defaultBarSettings : BarSettings = ./src/defaultBarSettings.dhall

let MarqueeSettings = ./src/MarqueeSettings.dhall

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(join : List Bar → Bar)
	  → λ(text : Text → Bar)
	  → λ(fg : Text → List Bar → Bar)
	  → λ(marquee : MarqueeSettings → List Bar → Bar)
	  → let space = text " "

		in  join [ text "Foo", text "bar", fg "red" [ text "moo" ] ]

in    [ { bar =
			mkSpec defaultBar : BarSpec
		, settings =
			defaultBarSettings : BarSettings
		}
	  ]
	: Configuration
