open Core
open Scanf

open Int64

let a, x, m = scanf "%Ld %Ld %Ld" Tuple3.create

let (+%) a b = (a + b) % m
let( *%) a b = (a * b) % m

let rec bin_power ?(acc=1L) p n =
  if n <= 0L then acc
  else
    let acc = if n % 2L <> 1L then acc else acc *% p in
    bin_power ~acc (p *% p) (n / 2L)

let rec solve = function
  | 0L -> 0L
  | x  ->
    if x % 2L = 1L then 1L +% a *% solve (x - 1L)
    else
      let x = x / 2L in (1L +% bin_power a x) *% solve x

let ans = solve x

let () = printf "%Ld\n%!" ans
