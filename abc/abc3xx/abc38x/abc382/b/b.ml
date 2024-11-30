open Core
open Scanf

let n, d = scanf "%d %d" Tuple2.create
let s = scanf " %s" String.to_array

let () =
  let i = ref (n - 1) in
  for _ = 0 to d - 1 do
    while Char.(s.(!i) = '.') do
      decr i
    done;
    s.(!i) <- '.';
    decr i
  done

let () =
  Array.iter s ~f:(fun c -> printf "%c" c);
  printf "\n%!"
