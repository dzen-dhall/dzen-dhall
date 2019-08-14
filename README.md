# dzen-dhall

[Dzen](https://github.com/robm/dzen) is a general purpose messaging, notification and menuing program for X11. It features rich in-text formating & control language, allowing to create GUIs by piping output of arbitrary executables to the `dzen2` binary. There are plenty of good usage examples on [r/unixporn](https://www.reddit.com/r/unixporn/search/?q=dzen&restrict_sr=1).

Unfortunately, combining outputs of multiple executables before feeding them to `dzen2`, which is usually done by custom shell scripts, is a tedious and error-prone task. Consider the following problems:

### Use of newlines

By default, dzen2 only renders the last line of its input, so newlines must be handled somehow by the programmer. When running in multiline mode (`-l` option), preserving correct output height is even more hard.

### Complexity of dynamic text formatting

If one wants each program's output to appear on its own fixed position on the screen, trimming and padding the output of each executable is required, to make sure that the text will not jitter when combined.

### High delays

Some output sources (shell scripts or commands used to provide the data) take too long to produce the output, some change their outputs rarely, but some are expected to update very frequently (like those that output current time or volume indicators on your screen). It means that the `while true; do ...; done | dzen2` pattern is not ideal. Some clever scheduling should be done to avoid delays and excessive resource waste. Output sources should be ran in parallel with their own update intervals.

### No code reuse

It is hard to share pieces of code used to produce output in dzen2 markup format because of the need to adapt the code. Ideally, there should be a "plugin system" allowing to import reusable configurations with a single command.

### Non-trivial markup is hard

Dzen markup language is quite rich: it features almost-arbitrary text positioning (using `^p` command), text coloring (`^fg`, `^bg`), drawing shapes (`^c`, `^co`, `^r`, `^ro`), loading XBM images (`^i`) and even allows to define clickable areas (`^ca`). However, these control structures are too low-level: implementing UI elements we want to use (for example, [marquee](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/marquee)-like blocks with arbitrary content) would require too much effort. Besides, one more problem with this markup language is that nested tags are not supported.

To fill the *abstraction gap*, new DSL should be introduced. This language should allow its users to abstract away from markup-as-text and focus on markup-as-[syntax-tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) instead - no need to say, tree structures are more suitable for the purpose of defining UIs. It is also way easier to process tree representations programmatically.

## The solution

[Dhall](https://dhall-lang.org/) is a statically-typed [total](https://en.wikipedia.org/wiki/Total_functional_programming) functional programming language. Its unique properties make it a good choice for dealing with complex user-defined configurations. Static typing allows to catch typos and errors early, and totality guarantees that the configuration program will never hang. Unlike some other configuration languages, it requires very little learning (even for people with no background in functional programming).

This repository contains data type and function definitions in Dhall that form a DSL for defining almost arbitrary Dzen UIs, called "bars", and a Haskell program capable of reading bar definitions and producing input for `dzen2` binary based on them.

The essence of the DSL can be illustrated by the following excerpt from [the default config file](dhall/config.dhall) (with additional comments):

```dhall
-- A bar that shows how much memory is used:
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
	  -- ^ And a multiline string containing a bash script

-- A bar that shows how much swap is used:
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
-- A bar that shows current time:
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

This definition results in the following Dzen output:

![Example 1](img/example1.png)

## Getting started

### Building

#### Using [stack](https://docs.haskellstack.org/en/stable/README/)

```
stack build
```

#### Using [Nix](https://nixos.org/nix/)

```
nix-build --attr dzen-dhall
```

To use pinned version of nixpkgs, pass `--arg usePinned true`.

### Installing

#### Using [stack](https://docs.haskellstack.org/en/stable/README/)

```
stack install
```

#### Using [Nix](https://nixos.org/nix/)

```
nix-env --file default.nix --install dzen-dhall
```

To use pinned version of nixpkgs, pass `--arg usePinned true`.

### Running

To create default configuration, run

```
dzen-dhall init
```

dzen-dhall will put some files to `~/.config/dzen-dhall/`

Files in `src/` and `lib/` subdirectories are set read-only by default - the user should not edit them, because they contain the implementation. They are still exposed to make it easier to debug the configuration.

## Installing plugins

`dzen-dhall` comes with a plugin system capable of pulling pieces of Dhall code with metadata either from a [curated set of plugins](https://github.com/dzen-dhall/plugins) or from third-party sources.

To install your first plugin, run...

TODO

This command will fetch the plugin source from TODO

It's recommended to check the downloaded source before proceeding. *Note that when fetching from remote, for maximum security you should review the file that was downloaded, and not just visit the link.*

You will see the following output:

TODO

This is a message the author left for you, to demonstrate how to actually use their plugin. Follow the instructions and edit your config file (which is usually located at `~/.config/dzen-dhall/config.dhall`) accordingly.

After saving the file, run `dzen-dhall` again. You should be able to see the output of a newly installed plugin, or a descriptive error message if something went wrong during the previous step.

## Creating plugins


<details><summary><strong>I want to make my Bar react to events</strong></summary>
<p>

Do you want your bar to be stateful or not? Stateful bar can react to the same events differently, depending on it's internal state, that can only be changed as a result of some event (not necessarily triggered by the user). Stateful bars can react to events emitted by [sources](#sources) and [hooks](#hooks), while stateless ones can only react to user-triggered mouse events.

<details><summary><strong>I want a stateful Bar</strong></summary>
<p>

To create a stateful `Bar`, you'll need to define an [automaton](#automata) and set up event routing for it.

The following components are required:

- a [slot](#slots) - an "address" that will be used to route events.
- a [state transition table](#state-transition-table) - a table describing how the automaton should react to events.
- a [state map](#state-maps) - a mapping from automaton states to `Bar`s.
- an automaton ID - just a string, that can be used from [sources](#sources) to query your automaton state.

For example, the following piece of code defines a `Bar` that switches between two states.

```dhall
let mySwitcher : Bar =
	  let mySlot = "MY_SLOT" : Slot

	  let stt
		  : StateTransitionTable
		  = [ { slots =
				  [ mySlot ]
			  , hooks =
				  [] : List Hook
			  , events =
				  [ Event.Mouse Button.Left ]
			  , from =
				  [ "" ]
			  , to =
				  "1"
			  }
			, { slots =
				  [ mySlot ]
			  , hooks =
				  [] : List Hook
			  , events =
				  [ Event.Mouse Button.Left ]
			  , from =
				  [ "1" ]
			  , to =
				  ""
			  }
			]

	  let stateMap
		  : StateMap Bar
		  = [ { state = "", bar = text "hello!" }
			, { state = "1", bar = text "world!" }
			]

	  let myID = "MY_AUTOMATON"

	  in  listener mySlot (automaton myID stt stateMap)
```

A [listener](#listeners) awaits for mouse events and sends them to the slot. Two state transitions are bound to the left-click event on the same slot. The default state of the automaton is `""` (by convention).

</p>
</details>
<details><summary><strong>I don't need my bar to be stateful</strong></summary>
<p>

Use this `Bar` constructor:

```dhall
∀(ca : Button → Text → Bar → Bar)
```

Its semantics resemble that of the `^ca()` dzen2 markup command.

Example:

```dhall
(ca Button.Left "notify-send hello!" (text "Click me!"))
```

</p>
</details>

</p>
</details>

## Concepts

This chapter describes dzen-dhall DSL in depth. You can safely skip most of it if you are only interested in consuming already made plugins.

It's best to read the [Dhall wiki](https://github.com/dhall-lang/dhall-lang/wiki) to become familiar with Dhall syntax before reading this chapter.

### Bar

The most important concept is `Bar`. Essentially, `Bar` is a tree containing text, images, shapes, etc. in its leaves.

The definition of `Bar` is the following:

```dhall
let Bar =
      ∀(Bar : Type)
    -- Text
    → ∀(text : Text → Bar)
    → ∀(raw : Text → Bar)

    -- Used to combine multiple Bars into one.
    → ∀(join : List Bar → Bar)

    -- Primitives of Dzen markup language.
    → ∀(fg : Color → Bar → Bar)
    → ∀(bg : Color → Bar → Bar)
    → ∀(i : Image → Bar)
    → ∀(r : Natural → Natural → Bar)
    → ∀(ro : Natural → Natural → Bar)
    → ∀(c : Natural → Bar)
    → ∀(co : Natural → Bar)
    → ∀(p : Position → Bar → Bar)
    → ∀(pa : AbsolutePosition → Bar → Bar)
    → ∀(ca : Button → Text → Bar → Bar)
    → ∀(ib : Bar → Bar)

    -- Animations
    → ∀(slider : Slider → List Bar → Bar)
    → ∀(marquee : Marquee → Bar → Bar)

    -- Other
    → ∀(padding : Natural → Padding → Bar → Bar)
    → ∀(source : Source → Bar)
    → ∀(plugin : Plugin → Bar)
    → ∀(listener : Slot → Bar → Bar)
    → ∀(automaton : Text → StateTransitionTable → StateMap Bar → Bar)
    → Bar
in Bar
```

`text` is used to create `Bar`s containing static, escaped pieces of text. `raw`, on the contrary, does not escape the given text, so that if it does contain markup, it will be interpreted by dzen2.

`join` is used to concatenate multiple bars together.

Various primitives of dzen2 markup language are represented by the corresponding constructors (`fg`, `bg`, `i`, etc.). See [dzen2 README](https://github.com/robm/dzen) for details on them.

dzen-dhall provides two kinds of high-level animations: `slider`s and `marquee`s.

### Automata

Each Bar is essentialy a finite-state automaton. States are tagged by `Text` labels, and transitions are triggered by [events] (very much like in some functional reactive programming frameworks). In the trivial case, a bar has only one state: you can think of any static `Bar` as of automaton with a single state, the name of which is implicit.

A bar with more than one state can be defined by its state transition function (encoded as a table), a mapping from state labels to `Bar`s, which defines its visual representation for different states, and an identifier used to query current state of the automaton from [sources](#sources).

### [State Transition Table](dhall/src/StateTransitionTable.dhall)

State transition table is a list of cases, each describing a certain condition and a reaction to it. In run time, when some event occurs, dzen-dhall tries to find the first row in a table matching current state of the [automaton](#Automata), an event name and a [slot](#Slots) name to which the event was sent. If there is a matching row in a table, dzen-dhall executes the specified [hooks](#Hooks) one by one, and if all of them do not cancel the transition, the state of the automaton is changed to a new one.

```dhall
let StateTransitionTable
	: Type
	= List
	  { slots :
		  List Slot
	  , events :
		  List Event
	  , from :
		  List State
	  , to :
		  State
	  , hooks :
		  List Hook
	  }
```

For example, let's define a simple transition table with two states: "on" and "off".

```dhall
let stt = [ { slots: [ "slot1" ]
            , events: [ Event.Left ]
		    , from: [ "on" ]
			, to: [ "off" ]
			, hooks: [] : List Hook
			}
		  , { slots: [ "slot1" ]
            , events: [ Event.Left ]
		    , from: [ "off" ]
			, to: [ "on" ]
			, hooks: [] : List Hook
			}
		  ]
```

This state transition table, when coupled with a [state map](#state-maps) to form an [automaton](#automata) and subscribed to some [listener](#listeners) that awaits for mouse events and sends them to the "slot1" slot, will result in a clickable area that switches between two states as the user clicks on a certain area.

### [Slots](dhall/src/Slot.dhall)

Slots are used to route events throughout the interface: you can think of slots as of adresses from which events can be sent. Each slot is essentially a piece of `Text`:

```dhall
let Slot : Type = Text in Slot
```

### [Hooks](dhall/src/Hook.dhall)

Hooks allow to execute arbitrary commands before state transitions of automata. They can also be used to prevent state transitions from happening - the
`requiredExitCodes : Optional (List Natural)`
field allows to specify a list of allowed exit codes for the command. If `allowedExitCodes` is set `None : Optional (List Natural)`, hook will always succeed. If it is `Some ([] : List Natural)`, it will always fail.

```dhall
let Hook
	: Type
	= { command :
		  List Text
	  , input :
		  Optional Text
	  , allowedExitCodes :
		  Optional (List Natural)
	  }

in  Hook
```

For example, The following hook will succeed only if a certain file exists:

```dhall
let myHook : Hook =
  { command = "bash"
  , input = "[ -f ~/some-file ]"
  , allowedExitCodes = Some [ 0 ]
  }
```

### Sources

Sources serve two purposes:

- Generate text output for `Bar`s

- Emit [events]

```dhall
let Source : Type =
  { updateInterval : Optional Natural
  , command : List Text
  , input : Optional Text
  , escapeMode : { joinLines : Bool, escapeMarkup : Bool }
  }
```

<details><summary><strong>SHOW EXAMPLE</strong></summary>
<p>

For example, a simple clock plugin can be created as follows:

```dhall
let clocks : Source =
  { updateInterval = Some 1000
  , command = "date +%H:%M"
  , input = None : Optional Text
  , escapeMode = { joinLines = False, escapeMarkup = True }
  }
```

</p>
</details>

#### Events and sources

Sources can be used to control automata.

To query for current state of an automaton, it is sufficient to read the environment variable of the form `STATE_id`, where `id` part is the identifier of the [automaton](#automata).

<details><summary><strong>SHOW EXAMPLE</strong></summary>
<p>

```dhall
let emitter : Source =
  { updateInterval = Some 1000
  , command = "bash"
  , input = Some ''
    echo "''$STATE_MY_AUTOMATON"
    ''
  , escapeMode = { joinLines = False, escapeMarkup = True }
  }
```

</p>
</details>

To emit an [event](#events), `EMIT` environment variable can be used. It contains a path of an executable, which can be used to tell dzen-dhall that some event occured.

<details><summary><strong>SHOW EXAMPLE</strong></summary>
<p>

The following source emits an event every second:

```dhall
let emitter : Source =
  { updateInterval = Some 1000
  , command = "bash"
  , input = Some ''
    ''$EMIT MY_SLOT MyEvent
    ''
  , escapeMode = { joinLines = False, escapeMarkup = True }
  }
```

</p>
</details>

### [Events](dhall/src/Event.dhall)

Events can be emitted by mouse interactions with [listeners](#listeners), by [hooks](#hooks) and by [sources](#sources). Listeners can only emit mouse events, hooks can emit any events, and sources can only emit `Custom` events.

```dhall
let Button = < Left | Middle | Right | ScrollUp | ScrollDown | ScrollLeft | ScrollRight >

let Event = < Mouse : Button | Custom : Text >
```

### Scopes

Scopes are used for encapsulation, to ensure that slots, automata and listeners from different plugins are unable to communicate with each other. You should always enclose your plugins in a separate scope.

## Naming conventions

These conventions are not enforced by dzen-dhall and will never be. These are just an attempt to lower cognitive noise for users and plugin maintainers.

- [Slot](#slots) names should contain only capital letters, numbers and `_`: `SLOT_1`, `MY_SLOT`, etc.
- [Event](#events) names should be written camel-cased, first letter capitalized: `TimeHasCome`, `ButtonClicked`, etc.
- [Automata] `id`s should contain only capital letters, numbers and `_`.

## Troubleshooting

This section is dedicated to fixing problems with your dzen-dhall configurations.

### Marquee jittering

Jittering may appear if `fontWidth` parameter value is too large or too small. It can be fixed by specifying the width manually:

```dhall
[ { bar = ...
  , settings = defaultBarSettings ⫽ { fontWidth = Some 10 }
  }
]
```

After a few guesses, you should be able to get rid of jittering.

Another possible source of this problem is non-monospace font being used. Non-monospace fonts are not supported and will never be.

### Writing shell scripts in Dhall

String interpolation in Dhall syntactically conflicts with bash notation for array expansion and indexing. E.g. `${arr[ ix ]}` should be written as `"\${arr[ ix ]}"` (in a double-quoted string) or as `'' ''${arr[ ix ]} ''` in a multiline string (that is, `''` serves as both an escape sequence and a quote symbol). See [the specification](https://github.com/dhall-lang/dhall-lang/blob/master/standard/multiline.md) for details.

## Implementation details

Read this section if you want to understand how dzen-dhall works.

Dhall does not support recursive ADTs (which are obviously required to construct tree-like statusbar configurations), but there is a [trick](https://github.com/dhall-lang/dhall-lang/wiki/How-to-translate-recursive-code-to-Dhall) to bypass that, called [Boehm-Berarducci encoding](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html). [dhall/src/Bar.dhall](dhall/src/Bar.dhall) contains the encoded definition for the recursive data type representing status bars. On the stage of [config](dhall/config.dhall) processing, before mashalling the configuration structure into Haskell, it is first converted to a non-recursive data called [Plugin](dhall/src/Plugin.dhall), which is a list of [Token](dhall/src/Token.dhall)s. These tokens can be marshalled into Haskell, and then [parsed back](src/DzenDhall/Parser.hs) into a tree structure ([DzenDhall.Data.Bar](src/DzenDhall/Data.hs)).

Dzen-dhall then spawns threads for each output source (like shell script or binary) and processes the outputs as specified in the configuration.
