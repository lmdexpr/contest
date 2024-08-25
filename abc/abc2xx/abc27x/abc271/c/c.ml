open Core
open Scanf

let n = scanf "%d" ident

let not_has = Array.init (n+2) ~f:(const true)
let book_end = n + 1
let () =
  for _ = 1 to n do
    scanf " %d" @@ fun i -> not_has.(min i book_end) <- false
  done

let rec solve ?(read=0) n =
  if n < 0 then read - 1
  else
    let read = read + 1 in
    solve ~read @@ n - 1 - Bool.to_int not_has.(read)

let () = printf "%d\n%!" @@ solve n
