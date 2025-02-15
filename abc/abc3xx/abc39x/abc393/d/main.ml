open Core
open Scanf

let n = scanf " %d" Fn.id
let s = scanf " %s" String.to_array

let ones =
  Iter.(0 -- (n - 1))
  |> Iter.filter Char.(fun i -> s.(i) = '1')
  |> Iter.to_array

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.append [| init |] @@ Array.folding_map a ~init ~f:(paired f)

let cumsum = cumsum ~init:0 ~f:(+) ones

let n = Array.length ones

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.map (fun i ->
    let x = ones.(i) in
    let l = i in
    let r = n - l - 1 in
    let ys = cumsum.(l) - cumsum.(0) in
    let zs = cumsum.(n) - cumsum.(i+1) in
    (l - r) * x - l * (l + 1) / 2 - r * (r + 1) / 2 - ys + zs
  )
  |> Iter.min_exn

let () = printf "%d\n%!" ans
