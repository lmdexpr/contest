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

  let comb n r = 
    let rec loop z i =
      if r < i then z
      else loop ((n - r + i) / i * z) (i + one)
    in
    loop one one
end
module M = Modulo (struct include Int let modulo = 1_000_000_007 end)

let x, y = scanf "%d %d" Tuple2.create

let () =
  let print_exit n = printf "%d\n%!" n; exit 0 in
  for a = 0 to x + 1 do
    let b2 = x - a in
    let b = b2 / 2 in

    if b2 % 2 = 0 && 2 * a + b = y then
      print_exit @@ M.comb (a + b) a
  done

let () = printf "0\n%!"
