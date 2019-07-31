let Button = ./src/Button.dhall

let Event = ./src/Event.dhall

in  [ Event.Custom "some text", Event.Mouse Button.Left ]
