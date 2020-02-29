open Angstrom
open Core

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
            <|> string "has"

let word =
  take_till (function ' ' -> true | _ -> false)
  >>| Types.Symbol.intern

let symbol = word
let name   = symbol

let product = fail ""

let product_then_name =
  let f prod name = Types.{name = Some name ; prod} in
  f <$> product <*> string "with name" *> name

let name_then_product =
  let f name prod = Types.{name = Some name ; prod} in
  f <$> string "name" *> name
    <*> string "with" *> product

let product_no_name = fail ""

(* could be faster, a lot of back tracking has to be done! *)
let start_product_gen =  product_then_name
                     <|> name_then_product
                     <|> product_no_name

let start_generic p =
  many (p *> start_product_gen)

let start =
  start_generic (string "has")

let start_no_has =
  start_generic (return ())

let generic =
  let f x y = Set.singleton (module Types.Symbol) x, y in
  f <$> symbol
    <*> start

let generics =
  let f x y = Set.of_list (module Types.Symbol) x, y in
  char 's' *>
  (f <$> many_till symbol reserved <*> start)

let generic_parser =
  string "generic" *>
  (generics <|> generic)

let english_start =
  let f y = Set.empty (module Types.Symbol), y in
  f <$> start

let english =
  let statement name (generics, choices) =
    Types.{name; generics; choices}
  in
  (string "type" <|> string "Type")
  *> (statement <$> name
                <*> (generic_parser <|> english_start))


(* ******************************************************* *)
(* Main Program *)
(* ******************************************************* *)

let statement =  english
             <|> math

let program = many1 statement
