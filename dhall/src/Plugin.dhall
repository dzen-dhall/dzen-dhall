  ∀(Plugin : Type)
→ ∀(join : List Plugin → Plugin)
→ ∀(text : Text → Plugin)
→ ∀(fg : Text → List Plugin → Plugin)
→ ∀(marquee : ./MarqueeSettings.dhall → List Plugin → Plugin)
→ Plugin
