{- Converts 'Bar' to 'Plugin' using 'mkPlugin' in a list of 'Configuration's.
-}

let List/map = ./../lib/List/map.dhall

let RawConfigEntry : Type = { bar : ./Bar.dhall, settings : ./BarSettings.dhall }

let Configuration : Type = ./Configuration.dhall

let mkPlugin = ./mkPlugin.dhall

let mkConfigs
	: List RawConfigEntry → List Configuration
	= List/map
	  RawConfigEntry
	  ./Configuration.dhall
	  (λ(raw : RawConfigEntry) → { bar = mkPlugin raw.bar, settings = raw.settings })

in  mkConfigs
