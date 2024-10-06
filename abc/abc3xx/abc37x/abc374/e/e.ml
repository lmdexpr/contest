open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create

let s = Array.create ~len:n (0, 0)
let t = Array.create ~len:n (0, 0)
let () =
  for i = 0 to n - 1 do
    s.(i) <- scanf " %d %d" Tuple2.create;
    t.(i) <- scanf " %d %d" Tuple2.create;
  done

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(x + 1) 0
let () =
  for i = 0 to n - 1 do
    let (a, p), (b, q) = s.(i), t.(i) in

    for j = 0 to x do
      let update j v = dp.(i + 1).(j) <- max dp.(i + 1).(j) v in

      let v = dp.(i).(j) in
      update j v;

      if j + p <= x then update (j + p) (v + a);
      if j + q <= x then update (j + q) (v + b);
    done
  done

let w = Array.create ~len:(x + 1) 0
let () =
  for i = x downto 0 do
    w.(i) <- dp.(n).(i)
  done

let ans = Array.min_elt w ~compare |> Option.value_exn

let () = printf "%d\n%!" ans
