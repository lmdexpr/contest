open Core
open Scanf

let s = scanf "%s" Fn.id

let count = Array.create ~len:26 0
let () =
  String.iter s ~f:(fun c ->
    let i = Char.to_int c - Char.to_int 'a' in
    count.(i) <- count.(i) + 1
  );
  Array.sort ~compare count

let yes = Array.create ~len:101 0
let () =
  Array.iter count ~f:(fun c ->
    if c <> 0 then
      yes.(c) <- yes.(c) + 1
  )

let yes =
  Array.for_all yes ~f:(fun c -> c = 0 || c = 2)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
