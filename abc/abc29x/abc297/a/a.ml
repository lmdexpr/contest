open Core
open Scanf

let n, d = scanf "%d %d" Tuple2.create
let t = Array.init n ~f:(fun _ -> scanf " %d" ident)

let () =
  for i = 0 to n - 2 do
    if t.(i + 1) - t.(i) <= d then begin
      printf "%d\n%!" t.(i + 1);
      exit 0
    end
  done;
  printf "-1\n%!"
