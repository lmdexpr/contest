open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let _n, r, c = scanf " %d %d %d" Tuple3.create
let s = scanf " %s" String.to_list

let update (r, c) = function
  | 'N' ->  r + 1, c
  | 'S' ->  r - 1, c
  | 'W' ->  r,     c + 1
  | 'E' ->  r,     c - 1
  | _   -> assert false

let rec solve bonefire takahashi smokes = 
  let smokes = Set.add smokes bonefire in
  function
  | s :: tl -> 
    let takahashi = update takahashi s in
    let bonefire  = update bonefire  s in
    printf "%d" @@ Bool.to_int @@ Set.mem smokes takahashi;
    solve bonefire takahashi smokes tl
  | _ ->
    printf "\n"

let () =
  solve (0, 0) (r, c) SP.empty s
