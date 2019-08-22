{- `Address` is just a tagged ("newtyped", in Haskell terminology) piece of text.

Tagging is used to catch more errors during type checking, by preventing
unification with `Text` values of other domains.

`Address` values are meant to be constructed using `utils.mkAddress`.
-}
let Address : Type = < Address : Text > in Address
