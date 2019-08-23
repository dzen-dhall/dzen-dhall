let Hook = ../types/Hook.dhall

let Shell = ../types/Shell.dhall

let mkBashHook
	: Shell → Hook
	= λ(shell : Shell) → { command = [ "bash" ], input = shell } : Hook

in  mkBashHook
