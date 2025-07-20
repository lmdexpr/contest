open Core
open Scanf

let s = scanf " %s" String.to_array

let ans = Array.filter_mapi s ~f:(fun i -> function
  | '#' -> Some (i + 1)
  | _   -> None
)

let n = Array.length ans

let () =
  for i = 0 to n - 1 do
    if i % 2 = 0 then
      printf "%d,%d\n%!" ans.(i) ans.(i + 1)
  done
