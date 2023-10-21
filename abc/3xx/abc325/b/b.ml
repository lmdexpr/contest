open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let p = Array.init 24 ~f:(fun i ->
  Array.fold a ~init:0 ~f:(fun acc (w, x) ->
    let t = (x + i) % 24 in
    acc + w * Bool.to_int (9 <= t && t <= 17)
  )
)

let ans = Array.max_elt p ~compare |> Option.value_exn

let () = printf "%d\n%!" ans
