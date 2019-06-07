{- The intersperse function takes an element and a list and `intersperses' that element between the elements of the list. For example,

```
intersperse Natural 0 [ 1, 2, 3, 4 ] = [ 1, 0, 2, 0, 3, 0, 4 ]
```
-}
let List/intersperse
	: ∀(e : Type) → e → List e → List e
	=   λ(e : Type)
	  → λ(z : e)
	  → λ(l : List e)
	  → let null = https://prelude.dhall-lang.org/List/null

		let cons =
				λ(e : Type)
			  → λ(z : e)
			  → λ(x : e)
			  → λ(f : { b : Bool, acc : List e } → { b : Bool, acc : List e })
			  → λ(t : { b : Bool, acc : List e })
			  →       if null e t.acc

				then  f { b = True, acc = [ x ] }

				else  if t.b

				then  f (t ⫽ { acc = t.acc # [ z, x ] })

				else  f (t ⫽ { b = True })

		let nil = λ(e : Type) → λ(x : { b : Bool, acc : List e }) → x

		let res =
			  List/fold
			  e
			  l
			  ({ b : Bool, acc : List e } → { b : Bool, acc : List e })
			  (cons e z)
			  (nil e)

		in  (res { b = False, acc = [] : List e }).acc

in  List/intersperse
