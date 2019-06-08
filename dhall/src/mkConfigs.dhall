{- Converts 'Bar' to 'BarSpec' using 'mkSpec' in a list of 'Configuration's.
-}

let List/map = ./../lib/List/map.dhall

let RawConfigEntry : Type = { bar : ./Bar.dhall, settings : ./BarSettings.dhall }

let Configuration : Type = ./Configuration.dhall

let mkSpec = ./mkSpec.dhall

let mkConfigs
	: List RawConfigEntry → List Configuration
	= List/map
	  RawConfigEntry
	  ./Configuration.dhall
	  (λ(raw : RawConfigEntry) → { bar = mkSpec raw.bar, settings = raw.settings })

in  mkConfigs
