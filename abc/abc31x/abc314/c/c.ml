open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let s = scanf " %s" Fn.id
let c = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let next = Array.create ~len:(m+1) 'X'
let () =
  for i = n - 1 downto 0 do
    let c = c.(i) in
    if Char.(next.(c) = 'X') then
      next.(c) <- s.[i]
  done

let () =
  for i = 0 to n - 1 do
    let c = c.(i) in
    printf "%c" next.(c);
    next.(c) <- s.[i]
  done;
  printf "\n"
