open Core
open Scanf

let m = scanf "%d" Fn.id

let d = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let y = Array.sum (module Int) d ~f:Fn.id

let m, d =
  Array.fold_until d ~init:(1, (y + 1) / 2) ~finish:Fn.id ~f:(fun (i, x) d ->
    if x <= d then Stop (i, x) else Continue (i + 1, x - d)
  )

let () = printf "%d %d\n%!" m d
