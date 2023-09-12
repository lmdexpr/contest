open Core
open Scanf

let n = scanf "%d" Fn.id

let to_right = Hashtbl.create (module Int)
let to_left  = Hashtbl.create (module Int)
let xy = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let s  = scanf " %s" Fn.id
let () =
  let update f default = function
    | None   -> default
    | Some x -> f default x
  in
  for i = 0 to n - 1 do
    let x, y = xy.(i) in
    match s.[i] with
    | 'L' -> Hashtbl.update to_left  y ~f:(update max x)
    |  _  -> Hashtbl.update to_right y ~f:(update min x)
  done

let yes =
  Array.exists xy ~f:(fun (_, y) ->
    match Hashtbl.find to_right y, Hashtbl.find to_left y with
    | None, _ | _, None -> false
    | Some l, Some r    -> l < r
  )
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
