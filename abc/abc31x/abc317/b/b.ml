open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () = Array.sort a ~compare

let ans = Array.fold_until a ~init:(a.(0) - 1) ~finish:Fn.id ~f:(fun p a ->
  let open Continue_or_stop in
  if p + 1 <> a then Stop (p + 1) else Continue a
)

let () = printf "%d\n%!" ans
