let Bar = ./Bar.dhall

let Token = ./Token.dhall

let concat = http://prelude.dhall-lang.org/List/concat

in    λ(x : Bar)
	→ x
	  (List Token)
	  (λ(tokens : List (List Token)) → concat Token tokens)
	  (λ(text : Text) → [ Token.Raw text ])
	  (   λ(color : Text)
		→ λ(children : List (List Token))
		→   [ Token.Raw ("^fg(" ++ color ++ ")") ]
		  # concat Token children
		  # [ Token.Raw "^fg()" ]
	  )
	  (   λ(settings : ./MarqueeSettings.dhall)
		→ λ(children : List (List Token))
		→ concat Token children
	  )
