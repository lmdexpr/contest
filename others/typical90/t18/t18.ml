open Core

let t = Scanf.scanf "%d" ident
let l, x, y = Scanf.scanf " %d %d %d" Tuple3.create

let dip e =
  let rad = float e /. float t *. (2. *. Float.pi) in
  let l_2 = float l /. 2. in
  let ny = -1. *. l_2 *. Float.sin rad in
  let dx = 0. -. float x
  and dy = ny -. float y in
  let dist = Float.(sqrt @@ square dx +. square dy) in
  let dz = l_2 -. l_2 *. Float.cos rad in
  Float.atan2 dz dist *. 180.0 /. Float.pi

let q = Scanf.scanf " %d" ident
let () =
  for _ = 1 to q do
    Scanf.scanf " %d" ident
    |> dip
    |> printf "%.12f\n%!"
  done
