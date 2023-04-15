open Core
open Scanf

let n = scanf "%d" ident

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let box  = Array.init (n+1)  ~f:(const SP.empty)
let card = Array.init 200001 ~f:(const Int.Set.empty)

let q  = scanf " %d" ident
let () = 
  for k = 1 to q do
    match scanf " %d" ident with
    | 1 ->
      let i, j = scanf " %d %d" Tuple2.create in
      box.(j)  <- SP.add box.(j) (i, k);
      card.(i) <- Int.Set.add card.(i) j
    | 2 ->
      let i = scanf " %d" ident in
      SP.iter box.(i) ~f:(fun (j, _) -> printf "%d " j); printf "\n%!"
    | 3 ->
      let i = scanf " %d" ident in
      Int.Set.iter card.(i) ~f:(printf "%d "); printf "\n%!"
    | _ -> ()
  done
