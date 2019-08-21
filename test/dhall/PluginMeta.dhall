let PluginMeta = ./types/PluginMeta.dhall

let pluginMeta =
	  { name =
		  "1"
	  , author =
		  "2"
	  , email =
		  Some "3"
	  , homepage =
		  Some "4"
	  , upstream =
		  Some "5"
	  , description =
		  "6"
	  , usage =
		  "7"
	  , apiVersion =
		  8
	  }

in  pluginMeta : PluginMeta
