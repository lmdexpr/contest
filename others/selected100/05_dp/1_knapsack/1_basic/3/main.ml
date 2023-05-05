(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DPL_1_C&lang=ja *)
open Scanf
open Printf

let n, w  = scanf "%d %d" (fun n w -> n, w)
let items = Array.init n (fun _ -> scanf " %d %d" (fun v w -> v, w))

let dp = Array.make_matrix (n + 1) (w + 1) 0
let () =
  items
  |> Array.iteri (fun i (vi, wi) ->
      for j = 0 to w do
        if j - wi >= 0 then
          dp.(i + 1).(j) <- max dp.(i).(j) (dp.(i + 1).(j - wi) + vi)
        else
          dp.(i + 1).(j) <- dp.(i).(j)
      done
    )

let ans = dp.(n).(w)

let () = printf "%d\n%!" ans
