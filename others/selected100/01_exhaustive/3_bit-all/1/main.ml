(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_5_A&lang=ja *)

open Scanf

let id x = x
let n = scanf "%d" id
let a = Array.init n @@ fun _ -> scanf " %d" id

let ans = Array.init 2001 @@ fun _ -> false
let () =
  for bits = 0 to 1 lsl n - 1 do
    ans.(
      Array.init n (fun i -> if bits land (1 lsl i) <> 0 then i else -1)
      |> Array.fold_left (fun acc i -> if i = -1 then acc else acc + a.(i)) 0
    ) <- true
  done

let () =
  let q = scanf " %d" id in
  for _ = 1 to q do
    let m = scanf " %d" id in
    print_endline @@ if ans.(m) then "yes" else "no"
  done
