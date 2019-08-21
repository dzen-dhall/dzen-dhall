{- A Boehm-Berarducci encoding for dzen2 bars.

Can be decoded to any type by providing "constructor functions" for each case.

For example, ./mkPlugin.dhall converts `Bar` to `Plugin`, which is a synonym for
 `List Token`.
-}

let Carrier = ./Carrier.dhall

let Bar = ∀(Bar : Type) → Carrier Bar → Bar

in  Bar
