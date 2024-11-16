open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let x = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let bonus = Array.create ~len:(n + 1) 0
let () =
  for _ = 1 to m do
    scanf " %d %d" (Array.set bonus)
  done

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) (-1)
let () =
  dp.(0).(0) <- 0;
  for i = 1 to n do
		for j = 1 to i do
      dp.(i).(j) <- dp.(i - 1).(j - 1) + x.(i - 1) + bonus.(j);
    done;

    dp.(i).(0) <- 0;

		for j = 0 to i - 1 do 
      dp.(i).(0) <- max dp.(i).(0) dp.(i - 1).(j);
    done;
  done

let ans = Array.fold dp.(n) ~init:0 ~f:Int.max

let () = printf "%d\n%!" ans
