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
  (* let ( let* ) l f = X.(l >>= f) *)
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

(* TODO :: update string to eat spaces after the letter! *)

let string = string

let string_ci = string_ci

let reserved =  string_ci "or"
            <|> string_ci "with"
            <|> string_ci "and"
            <|> string_ci "has"

let word =
  take_till (function ' ' -> true | _ -> false)
  >>| Types.Symbol.intern

let symbol = word
let name   = symbol

let type_parser =
  (string ":" <|> string_ci "of type")
  *> return []

let nam_gen =
  let+ name  = symbol
  and+ type' = type_parser
  in Types.{ name ; type' }

let naming =
  let+ name_type = nam_gen
  in Types.Naming name_type


let maybe_single =
  let+ name_type = nam_gen
  in Types.MaybeName name_type


(* TODO :: fix the `let rec` and `and` situation *)

let product =
  fix begin fun product ->
      let maybe_prod =
        let+ product = product in
        Types.Maybe { product }
      in

      let maybe =
        string_ci "maybe" *>
        (maybe_prod <|> maybe_single)
      in

      let prod = maybe <|> naming in

      let product_then_name =
        let+ prod = prod
        and+ name = string_ci "with name" *> name
        in Types.{name = Some name ; prod}
       in

       let product_no_name =
         let+ prod = prod in
         Types.{name = None ; prod}
       in

       let name_then_product =
         let+ name = string_ci "name" *> name
         and+ prod = string_ci "with" *> prod
         in Types.{name = Some name ; prod}

       (* could be faster, a lot of back tracking has to be done! *)
       in  product_then_name
       <|> name_then_product
       <|> product_no_name
    end


let start =
  many (string_ci "has" *> product)

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
  string_ci "generic" *>
  (generics <|> generic)

let english_start =
  let+ start = start in
  Set.empty (module Types.Symbol), start

let english =
  string_ci "type" *>
  let+ name                = name
  and+ (generics, choices) = generic_parser <|> english_start
  in Types.{name; generics; choices}

(* ******************************************************* *)
(* Main Program *)
(* ******************************************************* *)

let statement =  english
             <|> math

let program = many1 statement
