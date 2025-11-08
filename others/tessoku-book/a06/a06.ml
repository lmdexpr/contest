open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.append [| init |] @@ Array.folding_map a ~init ~f:(paired f)

let cumsum = cumsum ~init:0 ~f:(+) a

let () =
  for _ = 1 to q do
    let l = scanf " %d" Fn.id in
    let r = scanf " %d" Fn.id in
    let ans = cumsum.(r) - cumsum.(l - 1) in
    printf "%d\n" ans
  done
