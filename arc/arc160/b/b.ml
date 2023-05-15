open Core
open Scanf

let modulo = 998244353
let( + ) a b = (a + b) % modulo
let( * ) a b = a * b % modulo

let solve n =
  let r = float n |> sqrt |> int_of_float in
  let ans =
    Iter.(1 -- r)
    |> Iter.map (fun y ->
        6 * (y - 1) * (n / y - y) +
        3 * (n / y - y) +
        3 * (y - 1) +
        1
      )
    |> Iter.fold (+) 0
  in
  printf "%d\n" ans

let t = scanf "%d" ident
let () =
  for _ = 1 to t do
    scanf " %d" solve
  done
