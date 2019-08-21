{- Converts 'Bar' to 'Plugin' using 'mkPlugin' in a list of 'Configuration's.
-}

let prelude = ../prelude/package.dhall

let ConfigEntry
	: Type
	= { bar : ../types/Bar.dhall, settings : ../types/BarSettings.dhall }

let Configuration : Type = ../types/Configuration.dhall

let mkPlugin = ./mkPlugin.dhall

let mkConfigs
	: List ConfigEntry → List Configuration
	= prelude.List.map
	  ConfigEntry
	  Configuration
	  (   λ(entry : ConfigEntry)
		→ { bar = mkPlugin entry.bar, settings = entry.settings }
	  )

in  mkConfigs
