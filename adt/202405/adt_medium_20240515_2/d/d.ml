open Core
open Scanf

let s = Array.init 10 ~f:(fun _ -> scanf "%s\n" Fn.id)

let a = Array.find_mapi_exn ~f:(fun i x -> if String.(x <> "..........") then Some (i + 1) else None) s
let b = Array.find_mapi_exn ~f:(fun i x -> if String.(x <> "..........") then Some (10 - i) else None) @@ Array.rev s

let s = String.to_array s.(a - 1)

let c = Array.find_mapi_exn ~f:(fun i x -> if Char.(x <> '.') then Some (i + 1) else None) s
let d = Array.find_mapi_exn ~f:(fun i x -> if Char.(x <> '.') then Some (10 - i) else None) @@ Array.rev s

let () = 
  printf "%d %d\n" a b;
  printf "%d %d\n" c d;
