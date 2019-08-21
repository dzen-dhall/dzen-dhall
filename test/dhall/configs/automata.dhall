{- An example of how to define automata. -}
let prelude = ./prelude/package.dhall
let types = ./types/package.dhall
let utils = ./utils/package.dhall

let Address = types.Address
let Bar = types.Bar
let BarSettings = types.BarSettings
let Button = types.Button
let Carrier = types.Carrier
let Configuration = types.Configuration
let Event = types.Event
let Hook = types.Hook
let Slot = types.Slot
let StateMap = types.StateMap
let StateTransitionTable = types.StateTransitionTable
let mkConfigs = utils.mkConfigs
let defaultBarSettings : BarSettings = utils.defaultBarSettings

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(carrier : Carrier Bar)
	  → let text : Text → Bar = carrier.text
		let listener : Slot → Bar → Bar = carrier.listener
		let automaton
			: Address → StateTransitionTable → StateMap Bar → Bar
			= carrier.automaton

		let mySlot = "MY_SLOT" : Slot

		let stt
			: StateTransitionTable
			= [ { slots =
					[ mySlot ]
			  , hooks =
					[ { command =
						  [ "bash" ]
					  , input =
						  ''
						  notify-desktop $EVENT
						  ''
					  }
					]
				  : List Hook
		      , events =
			        [ Event.Mouse Button.Left ]
				, from =
					[ "" ]
				, to =
					"1"
				}
			  , { slots =
					[ mySlot ]
				, hooks =
					[] : List Hook
				, events =
					[ Event.Mouse Button.Left ]
				, from =
					[ "1" ]
				, to =
					""
				}
			  ]

		let stateMap
			: StateMap Bar
			= [ { state = "", bar = text "hello!" }
			  , { state = "1", bar = text "world!" }
			  ]

		let myID = "MY_AUTOMATON"

		in  listener mySlot (automaton myID stt stateMap)

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings : BarSettings }
	  ]
	: List Configuration
