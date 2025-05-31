open Core
open Scanf

let n = scanf " %d" Fn.id
let s = scanf " %d" Fn.id

let t = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes, _ =
  Array.fold t ~init:(true, 0) ~f:(fun (yes, p) t ->
    if t - p <= s then
      (yes, t)
    else
      (false, t)
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
