open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let b = Array.init n ~f:(fun _ -> scanf " %d" ident)

let c  = Array.zip_exn a b
let () = Array.sort c ~compare:(fun (a, _) (b, _) -> compare a b)

let _a, b = Array.unzip c

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let longest_increasing_seq =
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
  Array.fold ~f ~init:0

let ans = n + longest_increasing_seq b

let () = printf "%d\n%!" ans
