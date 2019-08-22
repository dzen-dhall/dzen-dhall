{- `Bar` to `Plugin` conversion. -}

let types = ../types/package.dhall

let prelude = ../prelude/package.dhall

let AbsolutePosition = types.AbsolutePosition

let Address = types.Address

let Bar = types.Bar

let Button = types.Button

let Carrier = types.Carrier

let Check = types.Check

let Color = types.Color

let Direction = types.Direction

let Hook = types.Hook

let Image = types.Image

let Marquee = types.Marquee

let OpeningTag = ../types/OpeningTag.dhall

let Padding = types.Padding

let Plugin = types.Plugin

let Position = types.Position

let Slider = types.Slider

let Source = types.Source

let State = types.State

let StateMap = types.StateMap

let StateTransitionTable = types.StateTransitionTable

let Token = ../types/Token.dhall

let enclose =
		λ(openingTag : OpeningTag)
	  → λ(child : Plugin)
	  → [ Token.Open openingTag ] # child # [ Token.Close ]

let List/intersperse = ./intersperse.dhall

let carrier
	: Carrier Plugin
	= { text =
		  λ(text : Text) → [ Token.Txt text ]
	  , markup =
		  λ(text : Text) → [ Token.Markup text ]
	  , join =
		  λ(children : List Plugin) → prelude.List.concat Token children
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
			( prelude.List.concat
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
	  , plug =
		  λ(p : Plugin) → p
	  , automaton =
			λ(address : Address)
		  → λ(stt : StateTransitionTable)
		  → λ(stateMap : StateMap Plugin)
		  → enclose
			(OpeningTag.Automaton { stt = stt, address = address })
			( prelude.List.concatMap
			  { state : State, bar : Plugin }
			  Token
			  (   λ(row : { state : State, bar : Plugin })
				→ enclose (OpeningTag.StateMapKey row.state) row.bar
			  )
			  stateMap
			)
	  , check =
		  prelude.List.map Check Token Token.Check
	  , define =
			λ(name : Text)
		  → λ(value : Text)
		  → [ Token.Define { name = name, value = value } ]
	  , scope =
		  enclose OpeningTag.Scope
	  }

let mkPlugin : Bar → Plugin = λ(constructor : Bar) → constructor Plugin carrier

in  mkPlugin
