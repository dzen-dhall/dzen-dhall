let Variable = ../types/Variable.dhall

let Shell = ../types/Shell.dhall

let showVariable
	: Variable → Shell
	= λ(variable : Variable) → merge { Variable = λ(text : Text) → text } variable

in  showVariable
