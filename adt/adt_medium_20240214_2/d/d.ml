open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let degree = Array.create ~len:n 0
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun _a b -> degree.(b - 1) <- degree.(b - 1) + 1
  done

let ans = Array.filter_mapi degree ~f:(fun i d -> if d = 0 then Some (i + 1) else None)

let () =
  match ans with
  | [| ans |] -> printf "%d\n%!" ans
  | _         -> printf "-1\n%!"
