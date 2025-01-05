open Core
open Scanf

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  open M

  let ( + ) a b = (a % modulo + b % modulo) % modulo
  let ( - ) a b = (a - b + modulo) % modulo
  let ( * ) a b = (a % modulo * b % modulo) % modulo

  let power a b =
    Array.init num_bits ~f:Fn.id
    |> Array.fold ~init:(one, a) ~f:(fun (p, q) i ->
      if b land (one lsl i) <> zero then p * q, q * q
      else
        p, q * q
    )
    |> Tuple2.get1
  let ( ** ) a b = power (a % modulo) (b % modulo)
end
module M = Modulo (struct include Int64 let modulo = 1_000_000_007L end)

let n, q = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)
let c = Array.init q ~f:(fun _ -> scanf " %d" Int.pred)

let dist = Array.init (n-1) ~f:M.(fun i -> a.(i) ** a.(Int.succ i))

let cumsum a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init:0L ~f:(paired M.(+))
let cumsum = Array.append [| 0L |] @@ cumsum dist

let dist from to_ =
  let r = max from to_ in
  let l = min from to_ in
  M.(cumsum.(r) - cumsum.(l))

let last, ans =
  Iter.of_array c
  |> Iter.fold M.(fun (p, acc) c -> c, acc + dist p c) (0, 0L)

let ans = M.(ans + dist last 0)

let () = printf "%Ld\n%!" ans
