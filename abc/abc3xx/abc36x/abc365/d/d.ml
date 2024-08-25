open Core
open Scanf

let n = scanf  "%d" Fn.id
let s = scanf " %s" String.to_array
  |> Array.map ~f:(function 'R' -> 0 | 'P' -> 1 | _ -> 2)

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:2 0

let ans =
  dp.(1).(1) <- 1;
  for i = 2 to n do
    let tie = s.(i - 2) in
    let won = (tie + 1) % 3 in

    let x = s.(i - 1) in
    if x = tie then
      dp.(i).(0) <- dp.(i - 1).(1)
    else if x = won then
      dp.(i).(0) <- dp.(i - 1).(0)
    else
      dp.(i).(0) <- max dp.(i - 1).(0) dp.(i - 1).(1);

    let x = (s.(i - 1) + 1) % 3 in
    if x = tie then
      dp.(i).(1) <- 1 + dp.(i - 1).(1)
    else if x = won then
      dp.(i).(1) <- 1 + dp.(i - 1).(0)
    else
      dp.(i).(1) <- 1 + max dp.(i - 1).(0) dp.(i - 1).(1);
  done;
  max dp.(n).(0) dp.(n).(1)

let () = printf "%d\n%!" ans
