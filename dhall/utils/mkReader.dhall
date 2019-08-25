{- A function that constructs a Bar that contains a source that reads a given
   variable repeatedly, with a specified interval (in milliseconds).

The following pieces of code are equivalent:

```dhall
bash 500
    ''
    echo "${get var}"
    ''
```

```dhall
reader var 500
```

-}
let types = ../types/package.dhall

let Variable = types.Variable

let Carrier = types.Carrier

let Bar = types.Bar

let showVariable = ./showVariable.dhall

let mkReader =
		λ(Bar : Type)
	  → λ(cr : Carrier Bar)
	  → λ(var : Variable)
	  → λ(updateInterval : Natural)
	  → cr.source
		{ command =
			[ "bash" ]
		, input =
			"\$GET ${showVariable var}"
		, updateInterval =
			Some updateInterval
		, escape =
			True
		}

in  mkReader
