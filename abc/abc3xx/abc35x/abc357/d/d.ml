open Core
open Scanf

let n = scanf "%Ld" Fn.id
let d = Int64.to_string n |> String.length |> Int64.of_int

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
    Array.init num_bits ~f:Fn.id
    |> Array.fold ~init:(one, a) ~f:(fun (p, q) i ->
      let ( * ) a b = (a % modulo * (b % modulo)) % modulo in
      if b land (one lsl i) <> zero then p * q, q * q
      else
        p, q * q
    )
    |> Tuple2.get1

  let ( + ) a b = ((a % modulo) + (b % modulo)) % modulo
  let ( - ) a b = ((a % modulo) - b + modulo) % modulo
  let ( * ) a b = ((a % modulo) * (b % modulo)) % modulo
  let ( / ) a b = a * inverse b
  let ( ** ) a b = power a b % modulo
end

module M = Modulo (struct include Int64 let modulo = 998244353L end)

let r = M.(10L ** d)

let ans = M.(
  n * ( (r ** n - 1L) / (r - 1L) )
  )

let () = printf "%Ld\n%!" ans
