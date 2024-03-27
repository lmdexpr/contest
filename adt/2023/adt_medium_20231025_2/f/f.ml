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

  let ( + ) a b = (a + b) % modulo
  let ( * ) a b = (a * b) % modulo
  let ( / ) a b = a * inverse b
end
module M998244353 = Modulo (struct include Int64 let modulo = 998244353L end)

let n = scanf "%Ld" Fn.id

let rec solve ?(acc=0L) pow10 =
  let open Int64 in
  let l = pow10 / 10L in
  let r = min n (pow10 - 1L) in
  if r < l then acc
  else
    let acc = M998244353.(
      let x = r - l + 1L in x * (x + 1L) / 2L + acc
    ) in
    solve ~acc (pow10 * 10L)

let ans = solve 10L 

let () = printf "%Ld\n%!" ans
