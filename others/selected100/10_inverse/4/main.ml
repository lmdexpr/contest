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
  let ( / ) a b = a * inverse b

  let ( + ) a b = (a % modulo + b % modulo) % modulo
  let ( * ) a b = (a % modulo * b % modulo) % modulo

  let ( - ) a b = (a % modulo - b % modulo + modulo) % modulo
end
module M = Modulo (struct include Int let modulo = 1_000_000_007 end)

let n  = scanf "%d" Fn.id

let m  = n - 1
let es = Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let ans = invalid_arg "TODO"

let () = printf "%d\n%!" ans
