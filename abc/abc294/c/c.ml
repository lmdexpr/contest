open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let b = Array.init m ~f:(fun _ -> scanf " %d" ident)

let rec solve ?(acc_a=Iter.empty) ?(acc_b=Iter.empty) ?(a_i=0) ?(b_i=0) i =
  if i >= n + m then acc_a, acc_b
  else
    let i = i + 1 in
    if a_i >= n then acc_a, Iter.append acc_b Iter.(i -- (n + m))
    else if b_i >= m then Iter.append acc_a Iter.(i -- (n + m)), acc_b
    else if a.(a_i) < b.(b_i) then solve ~acc_a:(Iter.snoc acc_a i) ~acc_b ~a_i:(a_i + 1) ~b_i i
    else
      solve ~acc_a ~acc_b:(Iter.snoc acc_b i) ~a_i ~b_i:(b_i + 1) i


let a, b = solve 0

let () =
  Iter.iter (printf "%d ") a; printf "\n%!";
  Iter.iter (printf "%d ") b; printf "\n%!";
