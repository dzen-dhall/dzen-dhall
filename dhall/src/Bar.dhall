{- A Boehm-Berarducci encoding for dzen2 bars.

Can be decoded to any type by providing "constructor functions" for each case.

For example, ./mkPlugin.dhall converts `Bar` to `List Token`.
-}
let Source = ./Source.dhall

let Marquee = ./Marquee.dhall

let Slider = ./Slider.dhall

let Plugin = ./Plugin.dhall

in    ∀(Bar : Type)
	→ ∀(join : List Bar → Bar)
	→ ∀(text : Text → Bar)
	→ ∀(fg : Text → List Bar → Bar)
	→ ∀(source : Source → Bar)
	→ ∀(marquee : Marquee → Bar → Bar)
	→ ∀(slider : Slider → List Bar → Bar)
	→ ∀(plugin : Plugin → Bar)
	→ Bar
