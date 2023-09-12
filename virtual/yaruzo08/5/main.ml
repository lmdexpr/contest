open Core
open Scanf

let n = scanf "%d" ident
let s = Array.init n ~f:(fun _ -> scanf " %s" ident)

let solve (count, ba, bx, xa, xx) s =
  let count = count + List.length (String.substr_index_all s ~may_overlap:false ~pattern:"AB") in
  match s.[0], s.[String.length s - 1] with
  | 'B', 'A' -> count, ba + 1, bx, xa, xx
  | 'B', _   -> count, ba, bx + 1, xa, xx
  | _, 'A'   -> count, ba, bx, xa + 1, xx
  | _, _     -> count, ba, bx, xa, xx + 1

let count, ba, bx, xa, _ = Array.fold s ~init:(0, 0, 0, 0, 0) ~f:solve

let ans =
  count +
  if ba = 0 then min xa bx
  else
    ba +
    match xa, bx with
    | 0, 0 -> -1
    | 0, _ -> 0
    | _, 0 -> 0
    | _, _ -> min xa bx

let () = printf "%d\n%!" ans
