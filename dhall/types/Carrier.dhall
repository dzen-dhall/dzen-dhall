let AbsolutePosition = ./AbsolutePosition.dhall

let Address = ./Address.dhall

let Button = ./Button.dhall

let Color = ./Color.dhall

let Check = ./Check.dhall

let Direction = ./Direction.dhall

let Hook = ./Hook.dhall

let Image = ./Image.dhall

let Marquee = ./Marquee.dhall

let Padding = ./Padding.dhall

let Plugin = ./Plugin.dhall

let Position = ./Position.dhall

let Slider = ./Slider.dhall

let Source = ./Source.dhall

let StateMap = ./StateMap.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

let Variable = ./Variable.dhall

let Carrier
	: ∀(Bar : Type) → Type
	=   λ(Bar : Type)
	  → { text :
			Text → Bar
		, markup :
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
		, pad :
			Natural → Padding → Bar → Bar
		, trim :
			Natural → Direction → Bar → Bar
		, source :
			Source → Bar
		, plug :
			Plugin → Bar
		, automaton :
			Address → StateTransitionTable → StateMap Bar → Bar
		, check :
			List Check → Bar
		, define :
			Variable → Text → Bar
		, scope :
			Bar → Bar
		}

in  Carrier
