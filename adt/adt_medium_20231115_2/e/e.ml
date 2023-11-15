open Core
open Scanf

let n = scanf "%d" Fn.id

let rec f ?(acc=Iter.empty) i =
  if i > n then acc
  else
    let acc = Iter.append acc (Iter.cons i acc) in
    f ~acc (i + 1)

let () = Iter.iter (printf "%d ") (f 1); printf "\n"
