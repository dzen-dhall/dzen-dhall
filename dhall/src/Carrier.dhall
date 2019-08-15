let AbsolutePosition = ./AbsolutePosition.dhall

let Assertion = ./Assertion.dhall

let Button = ./Button.dhall

let Color = ./Color.dhall

let Hook = ./Hook.dhall

let Image = ./Image.dhall

let Marquee = ./Marquee.dhall

let Padding = ./Padding.dhall

let Plugin = ./Plugin.dhall

let Position = ./Position.dhall

let Slider = ./Slider.dhall

let Slot = ./Slot.dhall

let Source = ./Source.dhall

let StateMap = ./StateMap.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

let Carrier
	: ∀(Bar : Type) → Type
	=   λ(Bar : Type)
	  → { text :
			Text → Bar
		, raw :
			Text → Bar
		, join :
			List Bar → Bar
		, fg :
			Color → Bar → Bar
		, bg :
			Color → Bar → Bar
		, i :
			Image → Bar
		, r :
			Natural → Natural → Bar
		, ro :
			Natural → Natural → Bar
		, c :
			Natural → Bar
		, co :
			Natural → Bar
		, p :
			Position → Bar → Bar
		, pa :
			AbsolutePosition → Bar → Bar
		, ca :
			Button → Text → Bar → Bar
		, ib :
			Bar → Bar
		, slider :
			Slider → List Bar → Bar
		, marquee :
			Marquee → Bar → Bar
		, padding :
			Natural → Padding → Bar → Bar
		, source :
			Source → Bar
		, plugin :
			Plugin → Bar
		, listener :
			Slot → Bar → Bar
		, automaton :
			Text → StateTransitionTable → StateMap Bar → Bar
		, check :
			List Assertion → Bar
		}

in  Carrier
