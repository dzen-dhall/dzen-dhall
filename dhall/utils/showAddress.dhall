let Address = ../types/Address.dhall

let showAddress
	: Address → Text
	= λ(address : Address) → merge { Address = λ(text : Text) → text } address

in  showAddress
