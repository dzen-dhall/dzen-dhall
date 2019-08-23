let Position
	: Type
	= < XY :
		  { x : Integer, y : Integer }
	  | _RESET_Y
	  | _LOCK_X
	  | _UNLOCK_X
	  | _LEFT
	  | _RIGHT
	  | _TOP
	  | _CENTER
	  | _BOTTOM
	  >

in  Position
