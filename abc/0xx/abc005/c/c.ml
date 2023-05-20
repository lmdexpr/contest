open Core
open Scanf

let t = scanf "%d" ident

let n = scanf " %d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let m = scanf " %d" ident
let b = Array.init m ~f:(fun _ -> scanf " %d" ident)

let yes =
  n >= m && 
  Array.fold_until a ~init:0 ~finish:(const false) ~f:(fun i a ->
      let open Continue_or_stop in
      let wait = b.(i) - a in
      let i = i + Bool.to_int (0 <= wait && wait <= t) in
      if i >= m then Stop true else Continue i
    )

let ans = if yes then "yes" else "no"
let () = printf "%s\n%!" ans
