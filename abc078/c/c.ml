open Core

let n, m = Scanf.scanf " %d %d" Tuple2.create

let time = 1900 * m + 100 * (n - m)

let rec pow2 ?(acc=1) k =
  if k = 0 then acc
  else
    pow2 ~acc:(acc * 2) (k - 1)

let () = printf "%d\n%!" @@ time * pow2 m
