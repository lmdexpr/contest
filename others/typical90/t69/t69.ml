open Core

open Int64

let n, k = Scanf.scanf "%Ld %Ld" Tuple2.create

let m = 1000000007L

let rec bin_power ?(acc=1L) p n m =
  if n <= 0L then acc
  else
    let acc = if n % 2L <> 1L then acc else acc * p % m in
    bin_power ~acc (p * p % m) (n / 2L) m

let ans =
  if k = 1L then if n = 1L then 1L else 0L
  else if n = 1L then k
  else
    (k * (k - 1L)) % m * bin_power (k - 2L) (n - 2L) m % m

let () = printf "%Ld\n%!" ans
