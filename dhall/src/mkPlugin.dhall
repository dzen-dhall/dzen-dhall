{- `Bar` to `Plugin` conversion. `Plugin` = `List Token`. -}

let AbsolutePosition = ./AbsolutePosition.dhall

let Assertion = ./Assertion.dhall

let Bar = ./Bar.dhall

let Button = ./Button.dhall

let Color = ./Color.dhall

let Hook = ./Hook.dhall

let Image = ./Image.dhall

let Marquee = ./Marquee.dhall

let OpeningTag = ./OpeningTag.dhall

let Padding = ./Padding.dhall

let Plugin = ./Plugin.dhall

let Position = ./Position.dhall

let Slider = ./Slider.dhall

let Slot = ./Slot.dhall

let Source = ./Source.dhall

let StateMap = ./StateMap.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

let Token = ./Token.dhall

let concat = ./../lib/List/concat.dhall

let List/intersperse = ./../lib/List/intersperse.dhall

let List/concatMap = ./../lib/List/concatMap.dhall

let List/map = ./../lib/List/map.dhall

let enclose =
		λ(openingTag : OpeningTag)
	  → λ(child : Plugin)
	  → [ Token.Open openingTag ] # child # [ Token.Close ]

let mkPlugin
	: Bar → Plugin
	=   λ(constructor : Bar)
	  → constructor
		(List Token)
		(λ(text : Text) → [ Token.Txt text ])
		(λ(raw : Text) → [ Token.Raw raw ])
		(λ(children : List Plugin) → concat Token children)
		(   λ(color : Color)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.FG color) child
		)
		(   λ(color : Color)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.BG color) child
		)
		(λ(image : Image) → [ Token.I image ])
		(λ(w : Natural) → λ(h : Natural) → [ Token.R { w = w, h = h } ])
		(λ(w : Natural) → λ(h : Natural) → [ Token.RO { w = w, h = h } ])
		(λ(radius : Natural) → [ Token.C radius ])
		(λ(radius : Natural) → [ Token.CO radius ])
		(λ(position : Position) → enclose (OpeningTag.P position))
		(λ(position : AbsolutePosition) → enclose (OpeningTag.PA position))
		(   λ(button : Button)
		  → λ(command : Text)
		  → enclose (OpeningTag.CA { button = button, command = command })
		)
		(enclose OpeningTag.IB)
		(   λ(slider : Slider)
		  → λ(children : List Plugin)
		  → enclose
			(OpeningTag.Slider slider)
			( concat
			  Token
			  (List/intersperse Plugin [ Token.Separator ] children)
			)
		)
		(   λ(marquee : Marquee)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.Marquee marquee) child
		)
		(   λ(width : Natural)
		  → λ(padding : Padding)
		  → enclose (OpeningTag.Padding { width = width, padding = padding })
		)
		(λ(source : Source) → [ Token.Source source ])
		(λ(p : Plugin) → p)
		(λ(slot : Slot) → enclose (OpeningTag.Listener slot))
		(   λ(id : Text)
		  → λ(stt : StateTransitionTable)
		  → λ(stateMap : StateMap Plugin)
		  → enclose
			(OpeningTag.Automaton { stt = stt, id = id })
			( List/concatMap
			  { state : Text, bar : Plugin }
			  Token
			  (   λ(row : { state : Text, bar : List Token })
				→ enclose (OpeningTag.StateMapKey row.state) row.bar
			  )
			  stateMap
			)
		)
		(List/map Assertion Token Token.Assertion)

in  mkPlugin
