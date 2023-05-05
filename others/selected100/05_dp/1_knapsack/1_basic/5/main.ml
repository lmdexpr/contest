(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_10_C&lang=ja *)
open Scanf
open Printf

let solve n x m y = 
  let dp = Array.make_matrix (n+1) (m+1) 0 in
  x |> String.iteri (fun i x ->
      y |> String.iteri (fun j y ->
          dp.(i+1).(j+1) <-
            if x = y then dp.(i).(j) + 1
            else
              max dp.(i).(j+1) dp.(i+1).(j)
        )
    );
  dp

let q = scanf "%d" (fun q -> q)
let () =
  for _ = 1 to q do
    let x = scanf " %s" (fun x -> x) and y = scanf " %s" (fun y -> y) in
    let n = String.length x and m = String.length y in
    let dp = solve n x m y in
    printf "%d\n" dp.(n).(m)
  done
