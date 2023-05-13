(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DPL_2_A&lang=ja *)
open Scanf
open Printf

let inf = 1_000_000_000

let n, m = scanf "%d %d" (fun n m -> n, m)
let g = Array.make_matrix n n inf
let () =
  for _ = 1 to m do
    let s, t, d = scanf " %d %d %d" (fun s t d -> s, t, d) in
    g.(s).(t) <- d
  done

let memorize f = 
  let memo = Array.make_matrix (1 lsl n + 1) n None in
  let rec g bits v =
    match memo.(bits).(v) with
    | Some v -> v
    | None   ->
      let result = f g bits v in
      memo.(bits).(v) <- Some result; result
  in
  g

let fold_min start stop =
  let rec fold_min i acc ~f =
    if i >= stop then acc
    else
      fold_min (i + 1) ~f @@ min acc (f i)
  in
  fold_min start inf

module Bits = struct
  let diff bits v = bits land (lnot (1 lsl v))
  let non  bits v = bits land (1 lsl v) = 0
end

let search start = memorize @@ fun self bits v ->
  if bits = 0 && v = start then 0
  else
    let f u =
      if Bits.non bits u then inf
      else if self Bits.(diff bits u) u >= inf then inf
      else
        g.(v).(u) + self Bits.(diff bits u) u
    in
    fold_min 0 n ~f

let ans = search 0 (1 lsl n - 1) 0
let ans = if ans >= inf then -1 else ans

let () = printf "%d\n%!" ans
