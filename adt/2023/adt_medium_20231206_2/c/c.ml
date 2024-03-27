open Core
open Scanf

let n, t = scanf "%d %d" Tuple2.create
let c = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let r = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let hash_tbl = Hashtbl.create (module Int)

let () =
  for i = 0 to n - 1 do
    Hashtbl.update hash_tbl c.(i) ~f:(function
      | None                       -> i+1, r.(i)      
      | Some (_, x) when x < r.(i) -> i+1, r.(i)
      | Some v                     -> v
    )
  done

let ans, _ =
  Hashtbl.find hash_tbl t
  |> Option.value ~default:(Hashtbl.find_exn hash_tbl c.(0))

let () = printf "%d\n%!" ans
