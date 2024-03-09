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
  let power a b =
    Array.init num_bits ~f:ident
    |> Array.fold ~init:(one, a) ~f:(fun (p, q) i ->
      if b land (one lsl i) <> zero then p * q % modulo, q * q % modulo
      else
        p, q * q % modulo
      )
    |> Tuple2.get1

  let ( + ) a b = (a + b) % modulo
  let ( * ) a b = (a * b) % modulo
  let ( / ) a b = a * inverse b
  let ( ** ) a b = power a b
end
module Mod998244353 = Modulo (struct include Int let modulo = 998244353 end)

let n, m = scanf "%d %d" Tuple2.create

let all = Mod998244353.(n ** m)
let dup =
  Iter.(1 -- (n-1))
  |> Iter.map (fun i -> Mod998244353.(i ** m))
  |> Iter.fold Mod998244353.(+) 0

let ans = all - dup

let () = printf "%d\n%!" ans
