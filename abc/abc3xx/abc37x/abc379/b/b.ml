open Core
open Scanf

let _, k = scanf "%d %d\n" Tuple2.create
let s    = scanf "%s\n" String.to_array 
  |> Array.map ~f:(function 'O' -> true | _ -> false)

let ans, _ = 
  Array.fold s ~init:(0, 0) ~f:(fun (ans, cnt) -> function
    | true when cnt = k - 1 -> ans + 1, 0
    | true                  -> ans, cnt + 1
    | false                 -> ans, 0
  )

let () = printf "%d\n%!" ans
