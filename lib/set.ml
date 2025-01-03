open Core

module type M = sig
  type t
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end
module Tuple2 = struct
  include Tuple2
  module Make (M1: M) (M2: M) = struct
    type t = M1.t * M2.t
    let compare = compare ~cmp1:M1.compare ~cmp2:M2.compare
    let sexp_of_t = sexp_of_t M1.sexp_of_t M2.sexp_of_t
    let t_of_sexp = t_of_sexp M1.t_of_sexp M2.t_of_sexp
  end
end
module S = Set.Make (Tuple2.Make (Int) (Int64))

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)
