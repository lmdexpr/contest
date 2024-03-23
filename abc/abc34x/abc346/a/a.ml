open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () = 
  for i = 0 to n - 2 do
    printf "%d %!" @@ a.(i) * a.(i + 1)
  done;
  printf "\n%!"
