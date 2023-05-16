(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DPL_1_D&lang=ja *)
open Scanf
open Printf

let n = scanf "%d" (fun x -> x)
let a = Array.init n (fun _ -> scanf " %d" @@ fun x -> x)

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans =
  let inf = 2_000_000_001 in
  let lis = Array.make (n+1) inf in
  let f last a =
    let j =
      if lis.(last) < a then last
      else
        binsearch (-1) n ~ok:(fun x -> a <= lis.(x))
    in
    lis.(j) <- a;
    last + if j = last then 1 else 0
  in
  Array.fold_left f 0 a

let () = printf "%d\n%!" ans
