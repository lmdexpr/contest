open Core
open Scanf

let modulo = 100000
let (+%) a b = (a + b) % modulo

let n, m = scanf "%d %d" Tuple2.create

let s = Array.init (n-1) ~f:(fun _ -> scanf " %d" Fn.id)
let a = Array.init m     ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum = 
  let cumsum a =
    let paired f a b = let r = f a b in r, r in
    Array.folding_map a ~init:0 ~f:(paired Int.(+))
  in
  Array.append [| 0 |] @@ cumsum s

let dist from to_ =
  let r = max from to_ in
  let l = min from to_ in
  cumsum.(r) - cumsum.(l)

let ans = 
  Array.fold a ~init:(0, 0) ~f:(fun (acc, from) a ->
    let to_ = from + a in
    let acc = acc +% dist from to_ in
    acc, to_
  )
  |> fst

let () = printf "%d\n%!" ans
