{- `Bar` to `Plugin` conversion. `Plugin` = `List Token`. -}

let AbsolutePosition = ./AbsolutePosition.dhall

let Bar = ./Bar.dhall

let Button = ./Button.dhall

let Carrier = ./Carrier.dhall

let Check = ./Check.dhall

let Color = ./Color.dhall

let Direction = ./Direction.dhall

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

let carrierListToken
	: Carrier (List Token)
	= { text =
		  λ(text : Text) → [ Token.Txt text ]
	  , raw =
		  λ(raw : Text) → [ Token.Raw raw ]
	  , join =
		  λ(children : List Plugin) → concat Token children
	  , fg =
			λ(color : Color)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.FG color) child
	  , bg =
			λ(color : Color)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.BG color) child
	  , i =
		  λ(image : Image) → [ Token.I image ]
	  , r =
		  λ(w : Natural) → λ(h : Natural) → [ Token.R { w = w, h = h } ]
	  , ro =
		  λ(w : Natural) → λ(h : Natural) → [ Token.RO { w = w, h = h } ]
	  , c =
		  λ(radius : Natural) → [ Token.C radius ]
	  , co =
		  λ(radius : Natural) → [ Token.CO radius ]
	  , p =
		  λ(position : Position) → enclose (OpeningTag.P position)
	  , pa =
		  λ(position : AbsolutePosition) → enclose (OpeningTag.PA position)
	  , ca =
			λ(button : Button)
		  → λ(command : Text)
		  → enclose (OpeningTag.CA { button = button, command = command })
	  , ib =
		  enclose OpeningTag.IB
	  , slider =
			λ(slider : Slider)
		  → λ(children : List Plugin)
		  → enclose
			(OpeningTag.Slider slider)
			( concat
			  Token
			  (List/intersperse Plugin [ Token.Separator ] children)
			)
	  , marquee =
			λ(marquee : Marquee)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.Marquee marquee) child
	  , pad =
			λ(width : Natural)
		  → λ(padding : Padding)
		  → enclose (OpeningTag.Padding { width = width, padding = padding })
	  , trim =
			λ(width : Natural)
		  → λ(direction : Direction)
		  → enclose (OpeningTag.Trim { width = width, direction = direction })
	  , source =
		  λ(source : Source) → [ Token.Source source ]
	  , plugin =
		  λ(p : Plugin) → p
	  , listener =
		  λ(slot : Slot) → enclose (OpeningTag.Listener slot)
	  , automaton =
			λ(id : Text)
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
	  , check =
		  List/map Check Token Token.Assertion
	  , scope =
		  enclose OpeningTag.Scope
	  }

let mkPlugin
	: Bar → Plugin
	= λ(constructor : Bar) → constructor (List Token) carrierListToken

in  mkPlugin
