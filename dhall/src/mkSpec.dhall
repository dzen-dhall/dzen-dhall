let Bar = ./Bar.dhall

let Token = ./Token.dhall

let SourceSettings = ./SourceSettings.dhall

let concat = http://prelude.dhall-lang.org/List/concat

in    λ(x : Bar)
	→ x
	  (List Token)
	  (λ(tokens : List (List Token)) → concat Token tokens)
	  (λ(text : Text) → [ Token.Txt text ])
	  (   λ(color : Text)
		→ λ(children : List (List Token))
		→   [ Token.Raw ("^fg(" ++ color ++ ")") ]
		  # concat Token children
		  # [ Token.Raw "^fg()" ]
	  )
	  (λ(ss : SourceSettings) → [ Token.Source ss ])
	  (   λ(settings : ./MarqueeSettings.dhall)
		→ λ(children : List (List Token))
		→ concat Token children
	  )
