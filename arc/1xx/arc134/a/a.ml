open Core
open Scanf

let n, l, w = scanf "%d %Ld %Ld" Tuple3.create
let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

open Int64

let (/^) a b = a / b + if a % b > 0L then 1L else 0L

let ans =
  Iter.snoc (Iter.of_array a) l
  |> Iter.scan   (fun (_, l) r -> l + w, r) (0L, -w)
  |> Iter.filter (fun (l, r) -> l < r)
  |> Iter.map    (fun (l, r) -> (r - l) /^ w)
  |> Iter.fold (+) 0L

let () = printf "%Ld\n%!" ans
