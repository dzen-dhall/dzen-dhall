let lib = ./lib/index.dhall
let types = ./src/types.dhall
let utils = ./src/utils.dhall

let AbsolutePosition = types.AbsolutePosition
let Assertion = types.Assertion
let Bar = types.Bar
let BarSettings = types.BarSettings
let Button = types.Button
let Button = types.Button
let Carrier = types.Carrier
let Check = types.Check
let Color = types.Color
let Configuration = types.Configuration
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
		let raw : Text → Bar = carrier.raw

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
		let padding : Natural → Padding → Bar → Bar = carrier.padding
		let source : Source → Bar = carrier.source
		let plugin : Plugin → Bar = carrier.plugin
		let listener : Slot → Bar → Bar = carrier.listener
		let automaton
			: Text → StateTransitionTable → StateMap Bar → Bar
			= carrier.automaton
		let check : List Assertion → Bar = carrier.check

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
				, input =
					Some input
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
					Assertion
					(   λ(binary : Text)
					  → { message = "", check = Check.BinaryInPath binary }
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
