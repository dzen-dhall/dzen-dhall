{-
Returns `True` if the `List` is empty and `False` otherwise

Examples:

```
./null Natural [ 0, 1, 2 ] = False

./null Natural ([] : List Natural) = True
```
-}
let null
    : ∀(a : Type) → List a → Bool
    = λ(a : Type) → λ(xs : List a) → Natural/isZero (List/length a xs)

in  null
