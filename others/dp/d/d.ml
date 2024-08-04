open Core
open Scanf

let n, w = scanf "%d %d" Tuple2.create
let item = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:100100 0

let chmax i j v = dp.(i).(j) <- max dp.(i).(j) v

let () =
  Array.iteri item ~f:(fun i (wi, vi) ->
    for sum = 0 to w do
      if sum - wi >= 0 then
        chmax (i + 1) sum @@ dp.(i).(sum - wi) + vi;

      chmax (i + 1) sum @@ dp.(i).(sum)
    done
  )

let ans = dp.(n).(w)

let () = printf "%d\n%!" ans
