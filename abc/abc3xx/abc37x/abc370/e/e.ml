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
    Array.init num_bits ~f:Fn.id
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
module M = Modulo (struct include Int64 let modulo = 998244353L end)

let n, k = scanf "%d %Ld" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let rec solve acc sum l r =
  if n <= r then 
    if Int64.(sum < k) then acc
    else
      let lL = Int64.of_int l in
      let nL = Int64.(of_int n - of_int r - 1L) in
      let acc = M.(acc + (2L ** lL) * (2L ** nL)) in
      let sum = Int64.(sum - a.(l)) in
      solve acc sum (l + 1) r
  else
    acc
  
let ks = solve 0L 0L 0 0

let ans = Int64.(2L ** (of_int n - 1L) - ks)

let () = printf "%Ld\n%!" ans
