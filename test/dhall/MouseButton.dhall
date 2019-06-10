let MouseButton = ./src/MouseButton.dhall

in  [ MouseButton.Left
	, MouseButton.Middle
	, MouseButton.Right
	, MouseButton.ScrollUp
	, MouseButton.ScrollDown
	, MouseButton.ScrollLeft
	, MouseButton.ScrollRight
	] : List MouseButton
