open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let log = Array.create ~len:(n + q + 2) (0, 0)
let () = 
  for i = 0 to n - 1 do
    log.(i) <- (n - i, 0)
  done

let rec solve i pos_head =
  if i > q then ()
  else
    match scanf " %d" Fn.id with
    | 1 ->
      let x, y = log.(pos_head) in
      let pos_head = pos_head + 1 in
      let x, y =
        match scanf " %c" Fn.id with
        | 'R' -> x + 1, y
        | 'L' -> x - 1, y
        | 'U' -> x, y + 1
        | _D  -> x, y - 1
      in
      log.(pos_head) <- x, y;
      solve (i + 1) pos_head
    | _ ->
      let p = scanf " %d" Fn.id in
      let x, y = log.(pos_head - p + 1) in
      printf "%d %d\n" x y;
      solve (i + 1) pos_head

let () = solve 1 (n - 1)
