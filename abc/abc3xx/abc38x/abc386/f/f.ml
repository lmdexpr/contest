open Core
open Scanf

let k = scanf "%d" Fn.id

let s = scanf " %s" Fn.id
let t = scanf " %s" Fn.id

let n = String.length s
let m = String.length t

let () =
  if k < abs (n - m) then (
    printf "No\n%!";
    exit 0
  )

(* レーベンシュタイン距離 (編集距離) DP / Levenshtein (edit) distance *)
let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(2 * k + 1) Int.max_value
let () =
  dp.(0).(k) <- 0;
  for i = 0 to n do
    for dj = 0 to 2 * k do

      let j = i + dj - k in
      let chmin v =
        dp.(i).(dj) <- min dp.(i).(dj) v
      in
      if 0 <= j && j <= m then (

        if 0 < i && dj < 2 * k then
          chmin @@ dp.(i - 1).(dj + 1) + 1;

        if 0 < j && 0 < dj then
          chmin @@ dp.(i).(dj - 1) + 1;

        if 0 < i && 0 < j then
          chmin @@ dp.(i - 1).(dj) + Bool.to_int Char.(s.[i - 1] <> t.[j - 1])
      )

    done
  done

let ans = if dp.(n).(k + m - n) <= k then "Yes" else "No"

let () = printf "%s\n%!" ans
