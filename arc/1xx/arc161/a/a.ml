open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let () =
  if n = 1 then begin
    printf "Yes\n%!";
    exit 0
  end;
  Array.sort ~compare a

let up   = Array.init (n / 2)     ~f:(fun i -> a.(i + n / 2 + 1))
let down = Array.init (n / 2 + 1) ~f:(fun i -> a.(i))

let yes =
  Array.for_alli up ~f:(fun i x -> down.(i) < x && x > down.(i + 1))

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
