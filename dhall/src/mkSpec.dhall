{- `Bar` to `BarSpec` conversion. `BarSpec` = `List Token`. -}
let Bar = ./Bar.dhall

let BarSpec = ./BarSpec.dhall

let Token = ./Token.dhall

let Source = ./Source.dhall

let OpeningTag = ./OpeningTag.dhall

let Marquee = ./Marquee.dhall

let Slider = ./Slider.dhall

let concat = ./../lib/List/concat.dhall

let List/intersperse = ./../lib/List/intersperse.dhall

let enclose =
		λ(openingTag : OpeningTag)
	  → λ(child : BarSpec)
	  → [ Token.Open openingTag ] # child # [ Token.Close ]

let mkSpec
	: Bar → BarSpec
	=   λ(constructor : Bar)
	  → constructor
		(List Token)
		(λ(tokens : List BarSpec) → concat Token tokens)
		(λ(text : Text) → [ Token.Txt text ])
		(   λ(color : Text)
		  → λ(children : List BarSpec)
		  →   [ Token.Raw ("^fg(" ++ color ++ ")") ]
			# concat Token children
			# [ Token.Raw "^fg()" ]
		)
		(λ(source : Source) → [ Token.Source source ])
		(   λ(marquee : Marquee)
		  → λ(child : BarSpec)
		  → enclose (OpeningTag.Marquee marquee) child
		)
		(   λ(slider : Slider)
		  → λ(children : List BarSpec)
		  → enclose
			(OpeningTag.Slider slider)
			( concat
			  Token
			  (List/intersperse (List Token) [ Token.Separator ] children)
			)
		)

in  mkSpec
