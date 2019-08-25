let VerticalDirection = ./VerticalDirection.dhall

let Fade = ./Fade.dhall

let Slider = { fadeIn : Fade, fadeOut : Fade, delay : Natural }

in  Slider
