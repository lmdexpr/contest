open Core
open Scanf

let n = scanf "%d" ident

let plan = Array.init n ~f:(fun _ -> scanf " %d %d %d" Tuple3.create)
let plan = Array.append [| (0, 0, 0) |] plan

let () =
  for i = 1 to n do
    let t, x, y = plan.(i - 1) and nt, nx, ny = plan.(i) in
    let dx = abs (nx - x) and dy = abs (ny - y) in
    let dt = nt - t - dx - dy in
    if dt < 0 || dt % 2 = 1 then (
      printf "No\n%!";
      exit 0
    )
  done

let () = printf "Yes\n%!"
