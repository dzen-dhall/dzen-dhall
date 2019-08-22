let types = ../types/package.dhall

let Button = types.Button

let showButton
	: Button → Text
	=   λ(button : Button)
	  → merge
		{ Left =
			"MouseLeft"
		, Middle =
			"MouseMiddle"
		, Right =
			"MouseRight"
		, ScrollUp =
			"MouseScrollUp"
		, ScrollDown =
			"MouseScrollDown"
		, ScrollLeft =
			"MouseScrollLeft"
		, ScrollRight =
			"MouseScrollRight"
		}
		button

in  showButton
