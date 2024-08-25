open Core
open Scanf

let h, w, m = scanf "%d %d %d" Tuple3.create
let query = Array.init m ~f:(fun _ -> scanf " %d %d %d" Tuple3.create)

let colors = Array.create ~len:(200_005) 0

let rows = Array.create ~len:h false
let cols = Array.create ~len:w false

let rec solve i hc wc =
  if i >= 0 then (
    let t, a, x = query.(i) in
    let a = a - 1 in
    let i = i - 1 in
    match t with
    | 1 when not rows.(a) -> 
      rows.(a) <- true;
      colors.(x) <- colors.(x) + wc;
      solve i (hc - 1) wc
    | 2 when not cols.(a) ->
      cols.(a) <- true;
      colors.(x) <- colors.(x) + hc;
      solve i hc (wc - 1)
    | _ -> solve i hc wc
  ) else
    colors.(0) <- colors.(0) + hc * wc

let () = solve (m - 1) h w

let ans =
  Array.filter_mapi colors
  ~f:(fun i c -> Option.some_if (c <> 0) (i, c))

let () =
  printf "%d\n%!" @@ Array.length ans;
  Array.iter ans ~f:(fun (i, c) -> printf "%d %d\n%!" i c)
