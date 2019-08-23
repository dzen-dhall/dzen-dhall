let Variable = ../types/Variable.dhall

let showVariable = ./showVariable.dhall

let Shell = ../types/Shell.dhall

let set
	: Variable → Text → Shell
	= λ(name : Variable) → λ(value : Text) → "\$SET ${showVariable name} \"${value}\""

in  set
