{- A type synonym for `Text` values that are used as shell scripts.

Unfortunately, `Shell`, unlike `State` or `Address`, can't be newtyped, because
we need to use string interpolation with it.
-}

let Shell : Type = Text in Shell
