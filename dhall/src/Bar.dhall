  ∀(Bar : Type)
→ ∀(join : List Bar → Bar)
→ ∀(text : Text → Bar)
→ ∀(fg : Text → List Bar → Bar)
→ ∀(marquee : ./MarqueeSettings.dhall → List Bar → Bar)
→ Bar
