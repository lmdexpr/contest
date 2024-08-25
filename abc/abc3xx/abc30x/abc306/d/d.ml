open Core
open Scanf

let n = scanf "%d" ident

let inf = 1_000_000_0

let dp = Array.init (n+1) ~f:(fun _ -> [| -inf; -inf |])
let () =
  dp.(0).(0) <- 0;
  for i = 1 to n do
    let x, y = scanf " %d %d" Tuple2.create in
    match x with
    | 0 ->
        dp.(i).(0) <- max dp.(i-1).(0) @@ max (dp.(i-1).(0) + y) (dp.(i-1).(1) + y);
        dp.(i).(1) <- dp.(i-1).(1);
    | _ ->
        dp.(i).(0) <- dp.(i-1).(0);
        dp.(i).(1) <- max dp.(i-1).(1) @@ max (dp.(i-1).(0) + y) dp.(i-1).(1);
  done

let ans = max dp.(n).(0) dp.(n).(1)

let () = printf "%d\n%!" ans
