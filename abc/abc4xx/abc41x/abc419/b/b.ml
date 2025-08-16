open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let q = scanf " %d" Fn.id

let rec solve set q =
  let q = q - 1 in
  if q < 0 then ()
  else
    match scanf " %d" Fn.id with
    | 1 -> 
      let x = scanf " %d" Fn.id in
      solve Set.(add set (x, q)) q
    | _ ->
      let ans = Set.min_elt_exn set in
      printf "%d\n%!" @@ fst ans;
      solve Set.(remove set ans) q

let () =
  solve SP.empty q
