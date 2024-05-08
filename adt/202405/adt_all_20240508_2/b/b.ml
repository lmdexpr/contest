open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init (7 * n) ~f:(fun _ -> scanf " %d" Fn.id)

let () = 
  for i = 0 to n - 1 do
    let sum = ref 0 in
    for j = 0 to 6 do
      sum := !sum + a.(i * 7 + j)
    done;
    printf "%d " !sum
  done;
  printf "\n"
