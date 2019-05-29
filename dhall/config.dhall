let Plugin : Type = ./src/Plugin.dhall

let Bar : Type = ./src/Bar.dhall

let makeBar : Plugin → Bar = ./src/makeBar.dhall

let BarSettings : Type = ./src/settings/bar/type.dhall

let defaultBarSettings : BarSettings = ./src/settings/bar/default.dhall

let defaultPlugin
	: Plugin
	=   λ(Plugin : Type)
	  → λ(join : List Plugin → Plugin)
	  → λ(text : Text → Plugin)
	  → λ(fg : Text → List Plugin → Plugin)
	  → λ(marquee : ./src/MarqueeSettings.dhall → List Plugin → Plugin)
	  → join [ text "Foo", text "bar", fg "red" [ text "moo" ] ]

in  [ { bar =
		  makeBar defaultPlugin : Bar
	  , settings = defaultBarSettings : BarSettings
	  }
	]
