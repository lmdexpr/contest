open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id) 

let mex a =
  Int.Set.of_array a
  |> Set.fold_until ~init:0 ~finish:Fn.id ~f:(fun acc x ->
    let open Continue_or_stop in
    if x = acc then Continue (acc + 1)
    else 
      Stop acc
  )
  |> min k

let ans = mex a

let () = printf "%d\n%!" ans
