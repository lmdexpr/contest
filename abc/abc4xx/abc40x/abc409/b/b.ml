open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  for x = 100 downto 0 do
    if x <= Array.count a ~f:(fun a -> x <= a) then (
      printf "%d\n%!" x;
      exit 0
    )
  done
