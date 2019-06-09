let lib = ./lib/index.dhall

let types = ./src/types.dhall

let utils = ./src/utils.dhall

let Configuration = types.Configuration

let BarSettings = types.BarSettings

let Bar = types.Bar

let Source = types.Source

let Marquee = types.Marquee

let Slider = types.Slider

let VerticalDirection = types.VerticalDirection

let Plugin = types.Plugin

let mkConfigs = utils.mkConfigs

let defaultBarSettings : BarSettings = utils.defaultBarSettings

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(join : List Bar → Bar)
	  → λ(text : Text → Bar)
	  → λ(fg : Text → List Bar → Bar)
	  → λ(source : Source → Bar)
	  → λ(marquee : Marquee → Bar → Bar)
	  → λ(slider : Slider → List Bar → Bar)
	  → λ(plugin : Plugin → Bar)
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

		in  separate
			[ join [ text "Mem: ", memoryUsage, text "%" ]
			, join [ text "Swap: ", swapUsage, text "%" ]
			, clocks
			]

in    mkConfigs
	  [ { bar = defaultBar : Bar, settings = defaultBarSettings : BarSettings }
	  ]
	: List Configuration
