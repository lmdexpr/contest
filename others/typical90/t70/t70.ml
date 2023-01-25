open Core

let n = Scanf.scanf "%d" ident

let xs = Array.init n ~f:(const 0)
let ys = Array.init n ~f:(const 0)
let () =
  for i = 0 to n - 1 do
    let x, y = Scanf.scanf " %d %d" Tuple2.create in
    xs.(i) <- x;
    ys.(i) <- y
  done;
  Array.sort xs ~compare;
  Array.sort ys ~compare

let x0, y0 =
  if n % 2 <> 0 then let i = (n - 1) / 2 in xs.(i), ys.(i)
  else
    let f a = (a.(n / 2) + a.(n / 2 - 1)) / 2 in
    f xs, f ys

let manhattan_dist a0 a = abs (a - a0)
let sum a0 = Array.sum (module Int) ~f:(manhattan_dist a0)

let () = printf "%d\n%!" @@ sum x0 xs + sum y0 ys
