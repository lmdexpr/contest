open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let () =
  for i = 0 to 9 do
    if a + b <> i then begin
      printf "%d\n" i;
      exit 0
    end
  done
