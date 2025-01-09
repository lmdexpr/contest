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

  (* nHr 重複組み合わせ combination with repetition / homogeneous *)
  let h n r = comb (n + r - one) r
end
module M = Modulo (struct include Int let modulo = 1_000_000_007 end)

let ans = scanf "%d %d" M.h

let () = printf "%d\n%!" ans
