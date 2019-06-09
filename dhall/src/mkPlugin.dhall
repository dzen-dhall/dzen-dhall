{- `Bar` to `Plugin` conversion. `Plugin` = `List Token`. -}
let Bar = ./Bar.dhall

let Plugin = ./Plugin.dhall

let Token = ./Token.dhall

let Source = ./Source.dhall

let OpeningTag = ./OpeningTag.dhall

let Marquee = ./Marquee.dhall

let Slider = ./Slider.dhall

let concat = ./../lib/List/concat.dhall

let List/intersperse = ./../lib/List/intersperse.dhall

let enclose =
		λ(openingTag : OpeningTag)
	  → λ(child : Plugin)
	  → [ Token.Open openingTag ] # child # [ Token.Close ]

let mkPlugin
	: Bar → Plugin
	=   λ(constructor : Bar)
	  → constructor
		(List Token)
		(λ(tokens : List Plugin) → concat Token tokens)
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
			  (List/intersperse (List Token) [ Token.Separator ] children)
			)
		)
		(λ(p : Plugin) → p)

in  mkPlugin