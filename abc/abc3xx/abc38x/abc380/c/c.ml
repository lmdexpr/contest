open Core
open Scanf

let _, k = scanf "%d %d" Tuple2.create
let s    = scanf " %s" String.to_list

let rec solve0 cnt acc = function
  | []        -> (cnt, 0) :: acc
  | '0' :: xs -> solve0 (cnt + 1) acc xs
  | xs        -> solve1 cnt 0 acc xs
and solve1 zc cnt acc = function
  | []        -> (zc, cnt) :: acc
  | '1' :: xs -> solve1 zc (cnt + 1) acc xs
  | xs        -> solve0 0 ((zc, cnt) :: acc) xs

let ans = solve0 0 [] s |> List.rev |> List.to_array
let () =
  let (zerok, onek)   = ans.(k - 1) in
  let (zerok1, onek1) = ans.(k - 2) in
  ans.(k - 1) <- zerok, 0;
  ans.(k - 2) <- zerok1, onek + onek1

let () =
  Array.iter ans ~f:(fun (z, o) ->
    for _ = 1 to z do printf "0" done;
    for _ = 1 to o do printf "1" done;
  );
  printf "\n%!"
