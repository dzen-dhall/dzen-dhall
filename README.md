# dzen-dhall

[Dzen](https://github.com/robm/dzen) is a general purpose messaging, notification and menuing program for X11. It features rich in-text formating & control language, allowing to create GUIs by piping output of arbitrary executables to the `dzen2` binary. There are plenty of good usage examples on [r/unixporn](https://www.reddit.com/r/unixporn/search/?q=dzen).

Unfortunately, combining outputs of multiple executables before feeding it to `dzen2`, which is usually done by custom shell scripts, is a tedious and error-prone task. Consider the following problems:

### Use of newlines

By default, dzen2 only renders the last line of its input, so newlines must be handled somehow by the user. When running in multiline mode (`-l` option), preserving correct output height is even more hard.

### Complexity of dynamic text formatting

If one wants each program's output to appear on its own fixed position on the screen, trimming and padding the output of each executable is required, to make sure that the text will not jitter when combined.

### High delays

Some output sources (shell scripts or commands used to provide the data) take too long to produce the output, some change their outputs rarely, but some are expected to update very frequently (like those that output current time or volume indicators on your screen). It means that the `while true; do ...; done | dzen2` pattern is not ideal. Some clever scheduling should be done to avoid delays and excessive resource waste. Output sources should be ran in parallel with their own update intervals.

### No code reuse

It is hard to share pieces of code used to produce output in dzen2 markup format because of the need to adapt the code. Ideally, there should be a "plugin system" allowing to import reusable configurations with a single command.

### Non-trivial markup is hard

Dzen markup language is quite rich: it features almost-arbitrary text positioning (using `^p` command), text coloring (`^fg`, `^bg`), drawing shapes (`^c`, `^co`, `^r`, `^ro`), loading XBM images (`^i`) and even allows to define clickable areas (`^ca`). However, these control structures are too low-level: implementing UI elements we want to use (for example, [marquee](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/marquee)-like blocks with arbitrary content) would require too much effort.

To fill in the *abstraction gap*, new DSL should be introduced. This language should allow its users to abstract away from markup-as-text and focus on markup-as-[syntax-tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) instead - no need to say, tree structures are more suitable for the purpose of defining UIs. It is also way easier to process tree representations programmatically.

## The solution

[Dhall](https://dhall-lang.org/) is a statically-typed [total](https://en.wikipedia.org/wiki/Total_functional_programming) programming language mostly used for dealing with complex user-defined configurations. This repository contains data type and function definitions in Dhall that form a DSL for defining almost arbitrary Dzen UIs, called "bars", and a Haskell program capable of reading bar definitions and producing input for `dzen2` binary based on them.

The essence of the DSL can be illustrated by the following excerpt from [the default config file](dhall/config.dhall) (with additional comments):

```dhall
let memoryUsage
-- ^ `let` keyword introduces new binding
	: Bar
	-- ^ Colon means "has type". `memoryUsage` is a `Bar`
	= bash
	  -- ^ Call to a function named `bash` with two arguments:
	  5000
	  -- ^ Update interval in milliseconds
	  ''
	  TMP=`free -b | grep 'Mem'`;
	  TMP=( $TMP );
	  TotalMem="''${TMP[ 1 ]}"
	  UsedMem="''${TMP[ 2 ]}"
	  echo "$((UsedMem * 100 / TotalMem))"
	  ''
	  -- ^ And a multiline string with script contents

let swapUsage
	: Bar
	= bash
	  5000
	  ''
	  TMP=`free -b | grep 'Swap'`;
	  TMP=( $TMP );
	  TotalSwap="''${TMP[ 1 ]}"
	  UsedSwap="''${TMP[ 2 ]}"
	  echo "$((UsedSwap * 100 / TotalSwap))"
	  ''

let clocks : Bar = bash 1000 "date +'%d.%m.%Y %A - %H:%M:%S'"

in  separate
    -- ^ a function that inserts a |-separator between nearby elements of a list
	[ join [ text "Mem: ", memoryUsage, text "%" ]
           -- ^ `text` is used to convert a text value to a `Bar`
	, join [ text "Swap: ", swapUsage, text "%" ]
	-- ^ `join` concatenates multiple `Bar`s
	, clocks
	] : Bar
```

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

Files in `src/` and `lib/` subdirectories are set read-only by default - the user should not edit them, because they contain the implementation. They are still exposed to make it easier to debug the configuration.

### Installing plugins

`dzen-dhall` comes with a plugin system capable of pulling pieces of Dhall code with metadata either from a [curated set of plugins](https://github.com/dzen-dhall/plugins) or from third-party sources.

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

## Implementation details

Read this section if you want to understand how dzen-dhall works.

Dhall does not support recursive ADTs (which are obviously required to construct tree-like statusbar configurations), but there is a [trick](https://github.com/dhall-lang/dhall-lang/wiki/How-to-translate-recursive-code-to-Dhall) to bypass that, called [Boehm-Berarducci encoding](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html). [dhall/src/Bar.dhall](dhall/src/Bar.dhall) contains the encoded definition for the recursive data type representing status bars. On the stage of [config](dhall/config.dhall) processing, before mashalling the configuration structure into Haskell, it is first converted to a non-recursive data called [Plugin](dhall/src/Plugin.dhall), which is a list of [Token](dhall/src/Token.dhall)s. These tokens can be marshalled into Haskell, and then [parsed back](src/DzenDhall/Parser.hs) into a tree structure ([DzenDhall.Data.Bar](src/DzenDhall/Data.hs)).

Dzen-dhall then spawns threads for each output source (like shell script or binary) and processes the outputs as specified in the configuration.
