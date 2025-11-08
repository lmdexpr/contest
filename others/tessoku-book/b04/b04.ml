open Core
open Scanf

let n = scanf "%s" Fn.id

let from_binary s =
  let n = String.length s in
  String.foldi s ~init:0 ~f:(fun i acc c ->
      acc + (if Char.(c = '1') then Int.pow 2 (n - i - 1) else 0)
    )

let ans = from_binary n

let () = printf "%d\n%!" ans
