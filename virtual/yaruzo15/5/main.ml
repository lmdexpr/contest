open Core
open Scanf

let k = scanf "%d" Fn.id

let a = Array.create ~len:(k+1) 0
let () =
  a.(1) <- 7 % k;
  for i = 2 to k do
    a.(i) <- (a.(i-1) * 10 + 7) % k
  done

let () = 
  for i = 1 to k do
    if a.(i) = 0 then begin
      printf "%d\n" i;
      exit 0
    end
  done;
  printf "-1\n"
