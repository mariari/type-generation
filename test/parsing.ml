open Core

let examples_english =
  "
Type foo with generic a
has network : a
and latency
and Thing of type Bar
with name Connected
or it has name Disconnected
with maybe (foo and boo : Boo with name bar)
and maybe baz : Maybe
"


let example_math =
  "
type foo a =
  | Connected {
      network : Foo;
      latency;
      thing   : Bar;
    }
  | Disconnected {
      maybe bar : { foo; boo : Boo }
    }
"

let example_both = String.concat ~sep:"/n" [example_math; example_math]
