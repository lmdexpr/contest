open Core
open Scanf

let n = scanf "%d" Fn.id

let () =
  for i = 0 to n do
    for j = 0 to n do
      for k = 0 to n do
        if i + j + k <= n then
          printf "%d %d %d\n" i j k
      done
    done
  done
