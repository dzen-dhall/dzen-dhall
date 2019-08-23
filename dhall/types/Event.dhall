{- `Event` is just a tagged ("newtyped", in Haskell terminology) piece of text.

Tagging is used to catch more errors during type checking, by preventing
unification with `Text` values of other domains.

`Event` values are meant to be constructed using `utils.mkEvent`.
-}
let Event = < Event : Text > in Event
