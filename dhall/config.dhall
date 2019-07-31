let lib = ./lib/index.dhall

let types = ./src/types.dhall

let utils = ./src/utils.dhall

let AbsolutePosition = types.AbsolutePosition

let Bar = types.Bar

let BarSettings = types.BarSettings

let Button = types.Button

let Event = types.Event

let Hook = types.Hook

let Color = types.Color

let Configuration = types.Configuration

let Image = types.Image

let Marquee = types.Marquee

let Button = types.Button

let Plugin = types.Plugin

let Position = types.Position

let Slider = types.Slider

let Slot = types.Slot

let Source = types.Source

let StateMap = types.StateMap

let StateTransitionTable = types.StateTransitionTable

let VerticalDirection = types.VerticalDirection

let mkConfigs = utils.mkConfigs

let defaultBarSettings : BarSettings = utils.defaultBarSettings

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(text : Text → Bar)
	  → λ(raw : Text → Bar)
	  → λ(join : List Bar → Bar)
	  → λ(fg : Color → Bar → Bar)
	  → λ(bg : Color → Bar → Bar)
	  → λ(i : Image → Bar)
	  → λ(r : Natural → Natural → Bar)
	  → λ(ro : Natural → Natural → Bar)
	  → λ(c : Natural → Bar)
	  → λ(co : Natural → Bar)
	  → λ(p : Position → Bar → Bar)
	  → λ(pa : AbsolutePosition → Bar → Bar)
	  → λ(ca : Button → Text → Bar → Bar)
	  → λ(ib : Bar → Bar)
	  → λ(slider : Slider → List Bar → Bar)
	  → λ(marquee : Marquee → Bar → Bar)
	  → λ(source : Source → Bar)
	  → λ(plugin : Plugin → Bar)
	  → λ(listener : Slot → Bar → Bar)
	  → λ(automaton : Text → StateTransitionTable → StateMap Bar → Bar)
	  → let separateBy =
				λ(sep : Bar)
			  → λ(list : List Bar)
			  → join (lib.List/intersperse Bar sep list)

		let separate = separateBy (text " | ")

		let bash
			: Natural → Text → Bar
			=   λ(interval : Natural)
			  → λ(input : Text)
			  → source
				{ command =
					[ "bash" ]
				, stdin =
					Some input
				, updateInterval =
					Some interval
				, escapeMode =
					{ joinLines = False, escapeMarkup = True }
				}

		let memoryUsage
			: Bar
			= bash
			  5000
			  ''
			  TMP=`free -b | grep 'Mem'`;
			  TMP=( $TMP );
			  TotalMem="''${TMP[ 1 ]}"
			  UsedMem="''${TMP[ 2 ]}"
			  echo "$((UsedMem * 100 / TotalMem))"
			  ''

		let swapUsage
			: Bar
			= bash
			  5000
			  ''
			  TMP=`free -b | grep 'Swap'`;
			  TMP=( $TMP );
			  TotalSwap="''${TMP[ 1 ]}"
			  UsedSwap="''${TMP[ 2 ]}"
			  echo "$((UsedSwap * 100 / TotalSwap))"
			  ''

		let clocks : Bar = bash 1000 "date +'%d.%m.%Y %A - %H:%M:%S'"

		let mySwitcher =
			  let mySlot = "MY_SLOT" : Slot

			  let stt
				  : StateTransitionTable
				  = [ { slots =
						  [ mySlot ]
					  , hooks =
						  [] : List Hook
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

		in  separate
			[ join [ text "Mem: ", memoryUsage, text "%" ]
			, join [ text "Swap: ", swapUsage, text "%" ]
			, clocks
			, mySwitcher
			]

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings : BarSettings }
	  ]
	: List Configuration
