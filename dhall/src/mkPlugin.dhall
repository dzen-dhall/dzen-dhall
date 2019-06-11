{- `Bar` to `Plugin` conversion. `Plugin` = `List Token`. -}
let Bar = ./Bar.dhall

let Plugin = ./Plugin.dhall

let Token = ./Token.dhall

let Source = ./Source.dhall

let OpeningTag = ./OpeningTag.dhall

let Marquee = ./Marquee.dhall

let Slider = ./Slider.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

let StateMap = ./StateMap.dhall

let concat = ./../lib/List/concat.dhall

let List/intersperse = ./../lib/List/intersperse.dhall
let List/concatMap = ./../lib/List/concatMap.dhall

let enclose =
		λ(openingTag : OpeningTag)
	  → λ(child : Plugin)
	  → [ Token.Open openingTag ] # child # [ Token.Close ]

let mkPlugin
	: Bar → Plugin
	=   λ(constructor : Bar)
	  → constructor
		(List Token)
		(λ(plugins : List Plugin) → concat Token plugins)
		(λ(text : Text) → [ Token.Txt text ])
		(   λ(color : Text)
		  → λ(children : List Plugin)
		  →   [ Token.Raw ("^fg(" ++ color ++ ")") ]
			# concat Token children
			# [ Token.Raw "^fg()" ]
		)
		(λ(source : Source) → [ Token.Source source ])
		(   λ(marquee : Marquee)
		  → λ(child : Plugin)
		  → enclose (OpeningTag.Marquee marquee) child
		)
		(   λ(slider : Slider)
		  → λ(children : List Plugin)
		  → enclose
			(OpeningTag.Slider slider)
			( concat
			  Token
			  (List/intersperse Plugin [ Token.Separator ] children)
			)
		)
		(λ(p : Plugin) → p)
		(   λ(stt : StateTransitionTable)
		  → λ(sm : StateMap Plugin)
		  → enclose
			(OpeningTag.Automaton stt)
			( List/concatMap
			  { state : Text, bar : Plugin }
			  Token
			  (   λ(row : { state : Text, bar : (List Token) })
				→ enclose (OpeningTag.StateMapKey row.state) row.bar
			  )
			  sm
			)
		)

in  mkPlugin
