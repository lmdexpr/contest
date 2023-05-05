(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DPL_1_A&lang=ja *)
open Scanf
open Printf

let n, m = scanf "%d %d" (fun n m -> n, m)
let c    = Array.init m (fun _ -> scanf " %d" (fun c -> c))

let dp = Array.make_matrix (m + 1) (n + 1) (n + 1)
let () =
  dp.(0).(0) <- 0;
  c |> Array.iteri (fun i c ->
      for j = 0 to n do
        if j - c >= 0 then
          dp.(i + 1).(j) <-
            min dp.(i).(j)
            @@ min
              (dp.(i).(j - c)     + 1)
              (dp.(i + 1).(j - c) + 1)
        else
          dp.(i + 1).(j) <- dp.(i).(j)
      done
    )

let ans = dp.(m).(n)

let () = printf "%d\n%!" ans
