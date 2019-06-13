{- A Boehm-Berarducci encoding for dzen2 bars.

Can be decoded to any type by providing "constructor functions" for each case.

For example, ./mkPlugin.dhall converts `Bar` to `Plugin`, which is a synonym for
 `List Token`.
-}

let Color = ./Color.dhall

let Image = ./Image.dhall

let Position = ./Position.dhall

let AbsolutePosition = ./AbsolutePosition.dhall

let Slider = ./Slider.dhall

let Marquee = ./Marquee.dhall

let Button = ./Button.dhall

let Source = ./Source.dhall

let Plugin = ./Plugin.dhall

let Hook = ./Hook.dhall

let StateTransitionTable = ./StateTransitionTable.dhall

let StateMap = ./StateMap.dhall

let Slot = ./Slot.dhall

let Bar =
      ∀(Bar : Type)
    -- Text
	→ ∀(text : Text → Bar)
	→ ∀(raw : Text → Bar)

	-- Used to combine multiple Bars into one.
	→ ∀(join : List Bar → Bar)

    -- Primitives of Dzen markup language.
    → ∀(fg : Color → Bar → Bar)
    → ∀(bg : Color → Bar → Bar)
    → ∀(i : Image → Bar)
    → ∀(r : Natural → Natural → Bar)
    → ∀(ro : Natural → Natural → Bar)
    → ∀(c : Natural → Bar)
    → ∀(co : Natural → Bar)
    → ∀(p : Position → Bar → Bar)
    → ∀(pa : AbsolutePosition → Bar → Bar)
    → ∀(ca : Button → Text → Bar → Bar)
    → ∀(ib : Bar → Bar)

    -- Animations
	→ ∀(slider : Slider → List Bar → Bar)
	→ ∀(marquee : Marquee → Bar → Bar)

    -- Other
	→ ∀(source : Source → Bar)
	→ ∀(plugin : Plugin → Bar)
	→ ∀(listener : Slot → Bar → Bar)
	→ ∀(automaton : Text → StateTransitionTable → StateMap Bar → Bar)
	→ Bar
in Bar
