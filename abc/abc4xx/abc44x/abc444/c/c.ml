open Core
open Scanf

let n = scanf " %d" Fun.id

let a  = Array.init n ~f:(fun _ -> scanf " %d" Fun.id)
let () = Array.sort a ~compare

let candidates =
  let m = Array.last a in
  if n mod 2 = 1 then Seq.return m
  else
    Seq.cons m Seq.(return @@ a.(0) + m)

let is_l l =
  let rest = Array.filter a ~f:(fun x -> x < l) in
  let k = Array.length rest in
  k mod 2 = 0
  && Array.for_alli rest ~f:(fun i x -> x + rest.(k - 1 - i) = l)

let ans = Seq.filter is_l candidates

let () =
  Seq.iter (printf "%d ") ans;
  printf "\n%!"
