let lib = ./lib/index.dhall
let types = ./src/types.dhall
let utils = ./src/utils.dhall

let AbsolutePosition = types.AbsolutePosition
let Address = types.Address
let Assertion = types.Assertion
let Bar = types.Bar
let BarSettings = types.BarSettings
let Button = types.Button
let Carrier = types.Carrier
let Check = types.Check
let Color = types.Color
let Configuration = types.Configuration
let Direction = types.Direction
let Event = types.Event
let Hook = types.Hook
let Image = types.Image
let Marquee = types.Marquee
let Padding = types.Padding
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
	  → λ(carrier : Carrier Bar)
	  → -- Text:
		let text : Text → Bar = carrier.text
		let markup : Text → Bar = carrier.markup

		-- Used to combine multiple Bars into one.
		let join : List Bar → Bar = carrier.join

		-- Primitives of Dzen markup language.
		let fg : Color → Bar → Bar = carrier.fg
		let bg : Color → Bar → Bar = carrier.bg
		let i : Image → Bar = carrier.i
		let r : Natural → Natural → Bar = carrier.r
		let ro : Natural → Natural → Bar = carrier.ro
		let c : Natural → Bar = carrier.c
		let co : Natural → Bar = carrier.co
		let p : Position → Bar → Bar = carrier.p
		let pa : AbsolutePosition → Bar → Bar = carrier.pa
		let ca : Button → Text → Bar → Bar = carrier.ca
		let ib : Bar → Bar = carrier.ib

		-- Animations
		let slider : Slider → List Bar → Bar = carrier.slider
		let marquee : Marquee → Bar → Bar = carrier.marquee

		-- Other
		let pad : Natural → Padding → Bar → Bar = carrier.pad
		let trim : Natural → Direction → Bar → Bar = carrier.trim
		let source : Source → Bar = carrier.source
		let plugin : Plugin → Bar = carrier.plugin
		let listener : Slot → Bar → Bar = carrier.listener
		let automaton
			: Address → StateTransitionTable → StateMap Bar → Bar
			= carrier.automaton
		let check : List Check → Bar = carrier.check
		let scope : Bar → Bar = carrier.scope

		let separateBy =
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
				, input = input
				, updateInterval =
					Some interval
				, escapeMode =
					{ joinLines = False, escapeMarkup = True }
				}

		let bashWithBinaries
			: List Text → Natural → Text → Bar
			=   λ(binaries : List Text)
			  → λ(interval : Natural)
			  → λ(input : Text)
			  → join
				[ check
				  ( lib.List/map
					Text
					Check
					(   λ(binary : Text)
					  → { message = "", assertion = Assertion.BinaryInPath binary }
					)
					binaries
				  )
				, bash interval input
				]

		let memoryUsage
			: Bar
			= bashWithBinaries
			  [ "free", "grep", "echo", "awk" ]
			  5000
			  ''
			  TMP=`free -b | grep 'Mem'`
			  TotalMem=`echo "$TMP" | awk '{ print $2; }'`
			  UsedMem=`echo "$TMP" | awk '{ print $3; }'`
			  echo "$((UsedMem * 100 / TotalMem))"
			  ''

		let swapUsage
			: Bar
			= bashWithBinaries
			  [ "free", "grep", "echo", "awk" ]
			  5000
			  ''
			  TMP=`free -b | grep 'Swap'`
			  TotalSwap=`echo "$TMP" | awk '{ print $2; }'`
			  UsedSwap=`echo "$TMP" | awk '{ print $3; }'`
			  echo "$((UsedSwap * 100 / TotalSwap))"
			  ''

		let clocks
			: Bar
			= bashWithBinaries [ "date" ] 1000 "date +'%d.%m.%Y %A - %H:%M:%S'"

		in  separate
			[ join [ text "Mem: ", memoryUsage, text "%" ]
			, join [ text "Swap: ", swapUsage, text "%" ]
			, clocks
			]

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings : BarSettings }
	  ]
	: List Configuration
