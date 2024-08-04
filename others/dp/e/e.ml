open Core
open Scanf

let n, w = scanf "%d %d" Tuple2.create
let item = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:100100 (1 lsl 30)

let chmin i j v = dp.(i).(j) <- min dp.(i).(j) v

let () =
  dp.(0).(0) <- 0

let () =
  Array.iteri item ~f:(fun i (wi, vi) ->
    for sum = 0 to 100100 - 1 do
      if sum - vi >= 0 then
        chmin (i + 1) sum @@ dp.(i).(sum - vi) + wi;

      chmin (i + 1) sum dp.(i).(sum)
    done
  )

let () = 
  for v = 100100 - 1 downto 0 do
    if dp.(n).(v) <= w then (
      printf "%d\n%!" v; exit 0
    )
  done
