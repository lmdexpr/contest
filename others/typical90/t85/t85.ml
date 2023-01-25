open Core
open Int64

let k = Scanf.scanf "%Ld" ident

let factorize n =
  let sqrt = Big_int.(big_int_of_int64 n |> sqrt_big_int |> int_of_big_int) in
  Iter.(1 -- sqrt)
  |> Iter.map of_int
  |> Iter.filter (fun p -> n % p = 0L)
  |> Iter.flat_map (fun p ->
      if p <> k / p then Iter.doubleton p (k / p) else Iter.singleton p
    )

let factors = factorize k |> Iter.to_array
let n = Int.(Array.length factors - 1)

let (let+) x k = Iter.flat_map k x
let (let^) x k = Iter.filter k x
let answers =
  let+ a = Iter.(0 -- n) in
  let^ b = Iter.(a -- n) in
  let a = factors.(a) in
  let b = factors.(b) in
  let c = k / a / b in
  a <= b && b <= c && k % (a * b) = 0L

let () = printf "%d\n%!" @@ Iter.length answers
