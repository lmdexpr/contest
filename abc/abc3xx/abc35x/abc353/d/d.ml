open Core
open Scanf

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  open M

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
  let ( ** ) a b = power a b
end

module M998244353 = Modulo (struct include Int let modulo = 998244353 end)

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let s = Array.map a ~f:(fun x -> Int.to_string x |> String.length)

let sum = Array.folding_map a ~init:0 ~f:M998244353.(fun acc x -> acc + x, acc + x)

let pow10_sum = Array.folding_map s ~init:0
  ~f:M998244353.(fun acc x -> let x = 10 ** x in acc + x, acc + x)

let ans = 
  Iter.(0 -- (n - 1))
  |> Fn.flip Iter.fold 0
    M998244353.(fun acc i ->
      acc + a.(i) * (pow10_sum.(n - 1) - pow10_sum.(i)) + (sum.(n - 1) - sum.(i))
    )

let () = printf "%d\n%!" ans
