let SourceSettings = ./SourceSettings.dhall

in    ∀(Bar : Type)
	→ ∀(join : List Bar → Bar)
	→ ∀(text : Text → Bar)
	→ ∀(fg : Text → List Bar → Bar)
	→ ∀(source : SourceSettings → Bar)
	→ ∀(marquee : ./MarqueeSettings.dhall → List Bar → Bar)
	→ Bar
