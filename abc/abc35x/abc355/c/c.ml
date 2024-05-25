open Core
open Scanf

let n, t = scanf "%d %d" Tuple2.create

let col = Array.create ~len:n 0
let row = Array.create ~len:n 0
let diag = Array.create ~len:2 0

let () = 
  for t = 1 to t do
    let a = scanf " %d" Fn.id - 1 in
    let x, y = a / n, a % n in
    row.(x) <- row.(x) + 1;
    col.(y) <- col.(y) + 1;

    if x = y then
      diag.(0) <- diag.(0) + 1;
    if x = n - y - 1 then
      diag.(1) <- diag.(1) + 1;

    if row.(x) = n || col.(y) = n || diag.(0) = n || diag.(1) = n then begin
      printf "%d\n%!" t;
      exit 0
    end
  done;
  printf "-1\n%!"
