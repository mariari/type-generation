open Angstrom
open Core

(* ******************************************************* *)
(* Let Syntax *)
(* ******************************************************* *)

module type Let_syntax = sig
  type 'a t
  val ( *> )  : _ t -> 'a t -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module M (X : Let_syntax) = struct
  let both xs ys = X.((fun x y -> (x, y)) <$> xs <*> ys)
  let ( let* ) l f = X.(l >>= f)
  let ( let+ ) l f = X.(f <$> l)
  let ( and+ ) l f = both l f

  (* figure out how to desugar into *> or >> *)
  (* let- and- : both throw away their argument *)
  (* this is mainly done for efficiency reasons *)
end

open M (Angstrom)

(* ******************************************************* *)
(* Generic Parsing tools *)
(* ******************************************************* *)

let between p q = p *> q <* p

let parens p =  char '(' *> p <* char ')'

(* ******************************************************* *)
(* Math Parser *)
(* ******************************************************* *)

let math =
  fail "undefined"

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

let maybe_prod =
  parens
    (fail "")

let maybe_single =
  fail ""

let maybe =
  string "maybe" *> (maybe_prod <|> maybe_single)

let type_parser = fail ""

let naming =
  let+ name = symbol
  and+ type'  = type_parser
  in Types.Naming { name ; type' }

let product = maybe <|> naming

let product_then_name =
  let+ prod = product
  and+ name = string "with name" *> name
  in Types.{name = Some name ; prod}

let name_then_product =
  let+ name = string "name" *> name
  and+ prod = string "with" *> product
  in Types.{name = Some name ; prod}

let product_no_name =
  let+ prod = product in
  Types.{name = None ; prod}

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
  let+ sym   = symbol
  and+ start = start
  in Set.singleton (module Types.Symbol) sym, start

let generics =
  char 's' *>
  let+ syms  = many_till symbol reserved
  and+ start = start
  in Set.of_list (module Types.Symbol) syms, start

let generic_parser =
  string "generic" *>
  (generics <|> generic)

let english_start =
  let+ start = start in
  Set.empty (module Types.Symbol), start

let english =
  (string "type" <|> string "Type") *>
  let+ name                = name
  and+ (generics, choices) = generic_parser <|> english_start
  in Types.{name; generics; choices}

(* ******************************************************* *)
(* Main Program *)
(* ******************************************************* *)

let statement =  english
             <|> math

let program = many1 statement
