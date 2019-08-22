let Variable = ../types/Variable.dhall

let set
	: Text → Text → Text
	= λ(name : Variable) → λ(value : Text) → "\$SET ${name} \"${value}\""

in  set
