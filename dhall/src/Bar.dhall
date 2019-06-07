let SourceSettings = ./SourceSettings.dhall

let MarqueeSettings = ./MarqueeSettings.dhall

in    ∀(Bar : Type)
	→ ∀(join : List Bar → Bar)
	→ ∀(text : Text → Bar)
	→ ∀(fg : Text → List Bar → Bar)
	→ ∀(source : SourceSettings → Bar)
	→ ∀(marquee : MarqueeSettings → Bar → Bar)
	→ Bar
