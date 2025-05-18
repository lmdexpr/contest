open Core
open Scanf

let h = scanf " %d" Fn.id
let w = scanf " %d" Fn.id

let n = scanf " %d" Fn.id

let xy = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let q = scanf " %d" Fn.id

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let _x = scanf " %d" Fn.id in
      ()
    | _ ->
      let _y = scanf " %d" Fn.id in
      ()
  done
