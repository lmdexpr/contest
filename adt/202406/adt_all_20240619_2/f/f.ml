open Core
open Scanf

let n, w = scanf "%d %d" Tuple2.create

let cheese = Array.init n ~f:(fun _ -> scanf " %Ld %d" Tuple2.create)
let () = 
  Array.sort cheese ~compare:(fun (a1, _) (a2, _) -> Int64.compare a2 a1)

let ans, _ =
  Array.fold cheese ~init:(0L,w) ~f:(fun (acc,w) (a, b) ->
    Int64.(acc + a * of_int (Int.min b w)), 
    max 0 @@ w - min b w
  )

let () = printf "%Ld\n%!" ans
