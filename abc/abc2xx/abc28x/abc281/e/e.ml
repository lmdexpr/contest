open Core
open Scanf

let n, m, k = scanf "%d %d %d" Tuple3.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

module Set = struct
  include Set
  let add = Fn.flip add
  let remove = Fn.flip remove
end

let pa i = a.(i), i

let l = 
  Iter.(0 -- (m - 1)) |> Iter.fold (fun acc i -> Set.add (pa i) acc) SP.empty
let l, r = List.split_n (Set.to_list l) k

let ans0 = List.sum (module Int) l ~f:fst
let l,r  = SP.of_list l, SP.of_list r

let f (l, r, pred, ans) i =
  let r = Set.add (pa @@ i + m) r in
  if Set.mem l (pa i) then
    let min_r = Set.min_elt_exn r in
    let ansi = pred - a.(i) + fst min_r in
    Set.(l |> remove (pa i) |> add min_r), Set.remove min_r r, 
    ansi, Iter.snoc ans ansi
  else
    let r = Set.remove (pa i) r in
    match Set.max_elt l, Set.min_elt r with
    | Some (ml, _ as max_l), Some (mr, _ as min_r) when mr < ml ->
      let ansi = pred - ml + mr in
      Set.(l |> add min_r |> remove max_l), 
      Set.(r |> add max_l |> remove min_r), ansi, Iter.snoc ans ansi
    | _ -> 
      l, r, pred, Iter.snoc ans pred

let _, _, _, ans =
  Iter.(0 -- (n - m - 1)) |> Iter.fold f (l, r, ans0, Iter.singleton ans0)

let () =
  Iter.iter (printf "%d ") ans;
  printf "\n"
