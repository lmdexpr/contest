open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let s = scanf " %s" Fn.id
let t = scanf " %s" Fn.id

let count = Array.create ~len:(n+1) 0

let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun l r ->
    count.(l-1) <- count.(l-1) + 1;
    count.(r)   <- count.(r)   - 1;
  done;
  for i = 1 to n do
    count.(i) <- count.(i) + count.(i-1)
  done

let ans =
  String.init n ~f:(fun i ->
    if count.(i) % 2 = 0 then s.[i] else t.[i]
  )

let () = printf "%s\n%!" ans
