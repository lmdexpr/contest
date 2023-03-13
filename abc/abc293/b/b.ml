open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let uncall = Int.Set.of_list @@ List.range ~stop:`inclusive 1 n
let uncall =
  Array.foldi a ~init:uncall ~f:(fun i uncall a -> if Int.Set.mem uncall (i + 1) then Int.Set.remove uncall a else uncall)

let () =
  printf "%d\n" @@ Int.Set.length uncall;
  Int.Set.iter uncall ~f:(printf "%d ");
  printf "\n%!"
