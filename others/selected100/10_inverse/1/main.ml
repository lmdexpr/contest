open Core
open Scanf

module Modulo (M : sig 
  include Int_intf.S 
  val modulo : t 
  val print : t -> unit
end) = struct
  open M

  let ( + ) a b = (a % modulo + b % modulo) % modulo
  let ( * ) a b = (a % modulo * b % modulo) % modulo

  let rec inverse ?(b = modulo) ?(u = one) ?(v = zero) a =
    if b = zero then (u % modulo + modulo) % modulo
    else
      let t = a / b in
      let a, b = b, a - t * b in
      let u, v = v, u - t * v in
      inverse ~b ~u ~v a
  let ( / ) a b = a * inverse b

  let (!) =
    let rec fact acc = function
      | n when n = zero -> acc
      | n               -> fact (acc * n) (n - one)
    in
    fact one

  let comb n r = !n / (!r * !(n - r))
end
module M = Modulo (struct include Int let modulo = 1_000_000_007 let print = printf "%d\n" end)

let w, h = scanf "%d %d" @@ fun w h -> w - 1, h - 1

let ans = M.comb (h + w) w

let () = printf "%d\n%!" ans
