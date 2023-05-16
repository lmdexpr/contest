(* https://atcoder.jp/contests/abc006/tasks/abc006_4 *)
open Core
open Scanf

let n = scanf "%d" (fun x -> x)
let c = Array.init n ~f:(fun _ -> scanf " %d" @@ fun x -> x)

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans =
  let inf = 2_000_000_001 in
  let lis = Array.create ~len:(n+1) inf in
  let f last a =
    let j =
      if lis.(last) < a then last
      else
        binsearch (-1) n ~ok:(fun x -> a <= lis.(x))
    in
    lis.(j) <- a;
    last + if j = last then 1 else 0
  in
  Array.fold c ~init:0 ~f

let ans = n - ans

let () = printf "%d\n%!" ans
