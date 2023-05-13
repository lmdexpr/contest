(* https://atcoder.jp/contests/s8pc-1/tasks/s8pc_1_g *)
open Core
open Scanf

let inf = Int64.of_float 1e16

let n, m = scanf "%d %d" Tuple2.create

let g = Array.make_matrix ~dimx:n ~dimy:n inf
let r = Array.make_matrix ~dimx:n ~dimy:n 0L
let () =
  for _ = 1 to m do
    let s, t    = scanf " %d %d" @@ fun s t -> s - 1, t - 1 in
    let d, time = scanf " %Ld %Ld" Tuple2.create in
    g.(s).(t) <- d;
    g.(t).(s) <- d;
    r.(s).(t) <- time;
    r.(t).(s) <- time;
  done

let memorize f = 
  let memo = Array.make_matrix ~dimx:(1 lsl n + 1) ~dimy:n None in
  let rec g bits v =
    match memo.(bits).(v) with
    | Some v -> v
    | None   ->
      let result = f g bits v in
      memo.(bits).(v) <- Some result; result
  in
  g

module Bits = struct
  let diff bits v = bits land (lnot @@ 1 lsl v)
  let non  bits v = bits land (1 lsl v) = 0
end

let search = memorize @@ fun self bits v ->
  match bits, v with
  | 0, 0                          -> 0L, 1L
  | 0, _ | _ when Bits.non bits v -> inf, 0L
  | _ ->
    Iter.(0 -- (n - 1))
    |> Iter.fold (fun (ans, cnt) u ->
        let open Int64 in

        let d, c = self Bits.(diff bits v) u in
        let d    = d + g.(u).(v) in

        if d <= r.(u).(v) then
          if ans > d then d, c
          else if ans = d then ans, cnt + c
          else ans, cnt
        else
          ans, cnt
      )
      (inf, 0L)

let ans, cnt = search (1 lsl n - 1) 0

let () =
  if Int64.(ans >= inf) then printf "IMPOSSIBLE\n%!"
  else
    printf "%Ld %Ld\n%!" ans cnt
