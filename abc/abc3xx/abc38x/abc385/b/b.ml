open Core
open Scanf

let h, w = scanf  "%d %d" Tuple2.create
let x, y = scanf " %d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let t = scanf " %s" Fn.id

let x, y = x - 1, y - 1

let check x y =
  0 <= x && x < h && 0 <= y && y < w && Char.(s.(x).(y) <> '#')

let update x y c =
  if Char.(s.(x).(y) = '@') then
    (s.(x).(y) <- '.'; c + 1)
  else
    c

let x, y, c =
  String.fold t ~init:(x, y, 0) ~f:(fun (x, y, c) -> function
    | 'U' when check (x - 1) y -> x - 1, y, update (x - 1) y c
    | 'D' when check (x + 1) y -> x + 1, y, update (x + 1) y c
    | 'L' when check x (y - 1) -> x, y - 1, update x (y - 1) c
    | 'R' when check x (y + 1) -> x, y + 1, update x (y + 1) c
    | _ -> x, y, c
  )

let x, y = x + 1, y + 1

let () = printf "%d %d %d\n%!" x y c
