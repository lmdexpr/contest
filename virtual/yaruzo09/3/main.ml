open Core
open Scanf

let w = scanf "%s" ident

let alp = Array.create ~len:26 0
let () =
  String.iter w ~f:(fun c ->
      let i = Char.to_int c - Char.to_int 'a' in
      alp.(i) <- alp.(i) + 1
    )

let yes = Array.for_all alp ~f:(fun x -> x mod 2 = 0)
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
