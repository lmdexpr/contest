(* https://atcoder.jp/contests/abc134/tasks/abc134_e *)
open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let weakly_lis =
  let inf = 1 lsl 30 in
  let lis = Array.create ~len:(n+1) inf in
  let f last a =
    let j =
      if lis.(last) < a then last
      else
        binsearch (-1) n ~ok:(fun x -> a < lis.(x))
    in
    lis.(j) <- a;
    last + if j = last then 1 else 0
  in
  Array.fold ~init:0 ~f

let () =
  Array.rev_inplace a;
  printf "%d\n%!" @@ weakly_lis a
