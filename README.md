# dzen-dhall

[Dzen](https://github.com/robm/dzen) is a general purpose messaging, notification and menuing program for X11. It features rich in-text formating & control language, allowing to create GUIs by piping output of arbitrary executables to the `dzen2` binary. There are plenty of good usage examples on [r/unixporn](https://www.reddit.com/r/unixporn/search/?q=dzen).

Unfortunately, combining outputs of multiple executables before feeding it to dzen, which is usually done by shell scripts, is a tedious and error-prone task. Consider the following problems:

### Use of newlines

By default, dzen2 only renders the last line of input, so newlines must be handled somehow by the user. When running in multiline mode (`-l` option), preserving correct output height is even more hard.

### Complexity of dynamic text formatting

If one wants each program's output to appear on its own fixed position on the screen, trimming and padding the output of each executable is required, to make sure that the text will not jitter when combined.

### High delays

Some output sources (shell scripts or commands used to provide the data) take too long to produce the output, some change their outputs rarely, but some are expected to update very frequently (like those that output current time or volume indicators on your screen). That means that the `while true; do ...; done | dzen2` pattern is not ideal. Some clever scheduling should be done to avoid delays and excessive resource waste. Output sources should be ran in parallel with their own update intervals.

Obviously, all these issues can be handled with the use of one's `$LANGUAGE_OF_CHOICE`. My `$LANGUAGE_OF_CHOICE` is [Dhall](https://dhall-lang.org/). It is [total](https://en.wikipedia.org/wiki/Total_functional_programming) (in particular, always-terminating) and statically-typed, which makes it ideal for complex user-defined configurations.

On the backend, Haskell is used to read the configuration and do all the heavy lifting.

## Example

See [the default config file](dhall/config.dhall).

## Getting started

### Building

#### Using [Nix](https://nixos.org/nix/)

```
nix-build --attr dzen-dhall
```

To use pinned version of nixpkgs, pass `--arg usePinned true`.

#### Using [stack](https://docs.haskellstack.org/en/stable/README/)

```
stack build
```

### Installing

#### Using [Nix](https://nixos.org/nix/)

```
nix-env --file default.nix --install dzen-dhall
```

To use pinned version of nixpkgs, pass `--arg usePinned true`.

#### Using [stack](https://docs.haskellstack.org/en/stable/README/)

```
stack install
```

### Running

To create default configuration, run

```
dzen-dhall init
```

dzen-dhall will put some files to `~/.config/dzen-dhall/`

## Troubleshooting

### Marquee jittering

Jittering may appear if `fontWidth` parameter value is too large or too small. It can be fixed by specifying the width manually:

```dhall
[ { bar = ...
  , settings = defaultBarSettings ⫽ { fontWidth = Some 10 }
  }
]
```

Another possible source of this problem is non-monospace font being used.

### Writing shell scripts in Dhall

String interpolation in Dhall syntactically conflicts with bash notation for array expansion and indexing. E.g. `${arr[ ix ]}` should be written as `"\${arr[ ix ]}"` (in a double-quoted string) or as `'' ''${arr[ ix ]} ''` in a multiline string (that is, `''` serves as both an escape sequence and a quote symbol). See [the specification](https://github.com/dhall-lang/dhall-lang/blob/master/standard/multiline.md) for details.

Also, dhall-format does not behave well

## Implementation details

Read this section if you want to understand how dzen-dhall works.

Dhall does not support recursive ADTs (which are obviously required to construct tree-like statusbar configurations), but there is a [trick](https://github.com/dhall-lang/dhall-lang/wiki/How-to-translate-recursive-code-to-Dhall) to bypass that, called [Boehm-Berarducci encoding](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html). [dhall/src/Bar.dhall](dhall/src/Bar.dhall) contains the encoded definition for the recursive data type representing status bars. On the stage of [config](dhall/config.dhall) processing, before mashalling the configuration structure into Haskell, it is first converted to a non-recursive data called [BarSpec](dhall/src/BarSpec.dhall), which is a list of [Token](dhall/src/Token.dhall)s. These tokens can be marshalled into Haskell, and then [parsed back](src/DzenDhall/Parser.hs) into a tree structure ([DzenDhall.Data.Bar](src/DzenDhall/Data.hs)).

Dzen-dhall then spawns threads for each output source (like shell script or binary) and processes the outputs as specified in the configuration.
