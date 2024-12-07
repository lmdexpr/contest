open Core
open Scanf

let n = scanf "%d" Fn.id

let tv = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let ans, _ =
  Array.fold tv ~init:(0, 0) ~f:(fun (ans, now) (t, v) ->
    let ans = max 0 @@ ans - (t - now) in
    (ans + v, t)
  )

let () = printf "%d\n%!" ans
