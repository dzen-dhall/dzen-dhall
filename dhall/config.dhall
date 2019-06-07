let Configuration = ./src/Configuration.dhall

let Bar : Type = ./src/Bar.dhall

let BarSpec : Type = ./src/BarSpec.dhall

let mkSpec : Bar → BarSpec = ./src/mkSpec.dhall

let BarSettings : Type = ./src/BarSettings.dhall

let defaultBarSettings : BarSettings = ./src/defaultBarSettings.dhall

let SourceSettings : Type = ./src/SourceSettings.dhall

let MarqueeSettings = ./src/MarqueeSettings.dhall

let List/intersperse
	: ∀(e : Type) → e → List e → List e
	= ./lib/List/intersperse.dhall

let defaultBar
	: Bar
	=   λ(Bar : Type)
	  → λ(join : List Bar → Bar)
	  → λ(text : Text → Bar)
	  → λ(fg : Text → List Bar → Bar)
	  → λ(source : SourceSettings → Bar)
	  → λ(marquee : MarqueeSettings → Bar → Bar)
	  → let separateBy =
			  λ(sep : Bar) → λ(l : List Bar) → join (List/intersperse Bar sep l)

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

in  [ { bar =
		  mkSpec defaultBar : BarSpec
	  , settings =
		  defaultBarSettings : BarSettings
	  }
	] : List Configuration
