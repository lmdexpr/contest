(* sample case を通っていない *)

open Core
open Scanf

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  open M

  let rec inverse ?(b = modulo) ?(u = one) ?(v = zero) a =
    if b = zero then (u % modulo + modulo) % modulo
    else
      let t = a / b in
      let a, b = b, a - t * b in
      let u, v = v, u - t * v in
      inverse ~b ~u ~v a

  let ( + ) a b = ((a % modulo) + (b % modulo)) % modulo
  let ( - ) a b = ((a % modulo) - b + modulo) % modulo
  let ( * ) a b = ((a % modulo) * (b % modulo)) % modulo
  let ( / ) a b = a * inverse b
end
module M = Modulo (struct include Int let modulo = 998244353 end)

let n, k = scanf "%d %d" Tuple2.create

let p = M.(2 * (n - 1) / (n * n))
let q = M.(2 / (n * n))

let dp = Array.create ~len:(k + 1) 0
let () =
  dp.(0) <- 1;
  for i = 0 to k - 1 do
    dp.(i + 1) <- M.((1 - p) * dp.(i) + q * dp.(i))
  done

let ans = M.(dp.(k) + (n * (n + 1) / 2 - 1) * (1 - dp.(k)) / (n - 1))
  
let () = printf "%d\n%!" ans
