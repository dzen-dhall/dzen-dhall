let Button = ./types/Button.dhall

let Event = ./types/Event.dhall

in  [ Event.Custom "some text", Event.Mouse Button.Left ]
