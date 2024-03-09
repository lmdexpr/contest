open Core
open Scanf

let n = scanf "%d" Fn.id

let covered = Array.make_matrix ~dimx:101 ~dimy:101 false
let () =
  for _ = 1 to n do
    let a, b = scanf " %d %d" Tuple2.create in
    let c, d = scanf " %d %d" Tuple2.create in
    for x = a + 1 to b do
      for y = c + 1 to d do
        covered.(x).(y) <- true
      done
    done
  done

let ans = Array.fold covered ~init:0 ~f:(fun acc row ->
  acc + Array.count row ~f:Fn.id
)

let () = printf "%d\n%!" ans
