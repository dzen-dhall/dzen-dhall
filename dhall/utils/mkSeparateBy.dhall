let types = ../types/package.dhall

let mkSeparateBy =
		λ(Bar : Type)
	  → λ(carrier : types.Carrier Bar)
	  → λ(sep : Bar)
	  → λ(list : List Bar)
	  → carrier.join (./intersperse.dhall Bar sep list)

in  mkSeparateBy
