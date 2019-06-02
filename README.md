# dzen-dhall

[Dzen](https://github.com/robm/dzen) is a general purpose messaging, notification and menuing program for X11. It features rich in-text formating & control language, allowing to create GUIs by piping output of arbitrary executables to the `dzen2` binary. There are plenty of good usage examples on [r/unixporn](https://www.reddit.com/r/unixporn/search/?q=dzen).

Unfortunately, combining outputs of multiple executables before feeding it to dzen, which is usually done by shell scripts, is a tedious and error-prone task. Consider the following problems:

### Use of newlines

By default, dzen2 only renders the last line of input, so newlines must be handled somehow by the user. When running in multiline mode (`-l` option), preserving correct output height is even more hard.

### Complexity of dynamic text formatting

If one wants each program's output to appear on its own fixed position on the screen, trimming and padding the output of each executable is required, to make sure that the text will not jitter when combined.

### High delays

Some input sources (shell scripts or commands used to provide the data) take too long to produce the output, some change their outputs rarely, but some are expected to update very frequently (like those that output current time or volume indicators on your screen). That means that the `while true; do ...; done | dzen2` pattern is not ideal. Some clever scheduling should be done to avoid delays and excessive resource waste.

Obviously, all these issues can be handled with the use of one's `$LANGUAGE_OF_CHOICE`. My `$LANGUAGE_OF_CHOICE` is [Dhall](https://dhall-lang.org/). It is [total](https://en.wikipedia.org/wiki/Total_functional_programming) (in particular, always-terminating) and statically-typed, which makes it ideal for complex user-defined configurations.

On the backend, Haskell is used to read the configuration and do the heavy lifting.

## Example

## Implementation details
