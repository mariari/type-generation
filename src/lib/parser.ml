open Angstrom

(* ******************************************************* *)
(* Math Parser *)
(* ******************************************************* *)

let math = fail "undefined"

(* ******************************************************* *)
(* English parser *)
(* ******************************************************* *)

let reserved =  string "or"
            <|> string "with"
            <|> string "and"

let word = take_till (function ' ' -> true | _ -> false)

let generic = string "generic"

let generics = fail ""

let generic_parser = generic <|> generics

let english =
  (string "type" <|> string "Type")
  *> fail "undefined"


(* ******************************************************* *)
(* Main Program *)
(* ******************************************************* *)

let statement =  english
             <|> math

let program = many1 statement
