{- The intersperse function takes an element and a list and `intersperses' that element between the elements of the list. For example,

```
intersperse Natural 0 [ 1, 2, 3, 4 ] = [ 1, 0, 2, 0, 3, 0, 4 ]
```
-}
let List/intersperse
	: ∀(e : Type) → e → List e → List e
	=   λ(e : Type)
	  → λ(separator : e)
	  → λ(list : List e)
	  → let cons =
				λ(element : e)
			  → λ(continue : List e → List e)
			  → λ(step : List e)
			  → continue
				(       if ./null.dhall e step

				  then  [ element ]

				  else  step # [ separator, element ]
				)

		let nil = λ(x : List e) → x

		in  List/fold e list (List e → List e) cons nil ([] : List e)

in  List/intersperse
