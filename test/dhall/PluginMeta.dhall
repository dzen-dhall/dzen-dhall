let PluginMeta = ./src/PluginMeta.dhall

let pluginMeta =
	  { name =
		  "1"
	  , author =
		  "2"
	  , email =
		  Some "3"
	  , homepage =
		  Some "4"
	  , upstreamURL =
		  Some "5"
	  , description =
		  "6"
	  , usage =
		  "7"
	  , requiredBinaries =
		  [ "bash" ]
	  , apiVersion =
		  8
	  }

in  pluginMeta : PluginMeta
