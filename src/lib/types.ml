open Core

module Symbol : sig
  type t [@@deriving hash, compare, sexp]
  include Comparator.S with type t := t
  val intern : string -> t
  val show : t -> string
  val (=) : t -> t -> bool
end = struct
  module T = struct
    type t = string [@@deriving compare, hash, sexp]
  end
  include T
  include Comparator.Make(T)
  let intern x = x
  let show x = x
  let (=) = String.(=)
end


type symbol_set = (Symbol.t, Symbol.comparator_witness) Set.t

let empty_symbol = Set.empty (module Symbol)


type naming = {
  name  : Symbol.t;
  type' : Symbol.t option;
}

type product_contents =
  | Maybe of {
      product : product
    }
  | MaybeName of naming
  | Naming of naming

and product = {
  name : Symbol.t option;
  prod : product_contents;
}

type statement = {
  name     : Symbol.t;
  generics : symbol_set;
  choices  : product list;
}


type program = {
  types : statement list;
}
