open Core
open Scanf

let rec floor_sum n m a b =
  let a1, a2 = a / m, a % m in
  let b1, b2 = b / m, b % m in
  let s = n * (n - 1) / 2 * a1 in
  if a2 = 0 then
    s + b1 * n
  else
    let k = (a2 * (n - 1) + b2) / m in
    s + n * (k + b1) - floor_sum k a2 m (m + a2 - b2 - 1)

let t = scanf " %d" Fn.id
let () =
  for _ = 1 to t do
    scanf " %d %d %d %d" floor_sum
    |> printf "%d\n"
  done
