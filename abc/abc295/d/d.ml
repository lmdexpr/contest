open Core
open Scanf

let s = scanf "%s" String.to_array |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0')

let count = Array.init 1024 ~f:(const 0)

let ans =
  count.(0) <- 1;
  Array.fold s ~init:(0, 0) ~f:(fun (bits, ans) x ->
      let bits = bits lxor (1 lsl x) in
      let ans = ans + count.(bits) in
      count.(bits) <- count.(bits) + 1;
      bits, ans
    )
  |> Tuple2.get2

let () = printf "%d\n%!" ans
