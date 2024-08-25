open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let b = Array.init 27 ~f:(fun i ->
  let mask = 1 lsl i in
  mask, Array.map a ~f:(fun x -> if x land mask <> 0 then 1 else 0)
)

let cumsum a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init:0 ~f:(paired Int.bit_xor)

let ans =
  Array.fold b ~init:0 ~f:(fun acc (mask, b) ->
    let c = cumsum b in
    let ones = Array.count c ~f:(fun x -> x = 1) in
    let zero = n - ones + 1 in
    acc + mask * (zero * ones - Array.sum (module Int) b ~f:Fn.id)
  )

let () = printf "%d\n%!" ans
