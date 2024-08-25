open Core
open Scanf

let rec solve prev =
  if prev = 0 then printf "0\n"
  else begin
    let a = scanf " %d" Fn.id in
    solve a;
    printf "%d\n%!" prev
  end

let () =
  scanf "%d" solve
