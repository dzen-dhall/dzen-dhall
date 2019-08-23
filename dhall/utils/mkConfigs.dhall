{- Converts 'Bar' to 'Plugin' using 'mkPlugin' in a list of 'Configuration's.
-}

let prelude = ../prelude/package.dhall

let types = ../types/package.dhall

let Bar = types.Bar

let Configuration = types.Configuration

let Settings = types.Settings

let ConfigEntry = { bar : Bar, settings : Settings }

let mkPlugin = ./mkPlugin.dhall

let mkConfigs
	: List ConfigEntry → List Configuration
	= prelude.List.map
	  ConfigEntry
	  Configuration
	  (λ(entry : ConfigEntry) → entry ⫽ { bar = mkPlugin entry.bar })

in  mkConfigs
