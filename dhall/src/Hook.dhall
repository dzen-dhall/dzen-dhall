let Hook
	: Type
	= { command :
		  List Text
	  , stdin :
		  Optional Text
	  , allowedExitCodes :
		  Optional (List Natural)
	  }

in  Hook
