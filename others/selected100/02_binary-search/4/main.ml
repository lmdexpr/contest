(* https://atcoder.jp/contests/abc023/tasks/abc023_d *)
open Core
open Scanf

let n = scanf "%d" ident
let h = Array.init n ~f:(const 0L)
let s = Array.init n ~f:(const 0L)
let () =
  for i = 0 to n - 1 do
    let hi, si = scanf " %Ld %Ld" Tuple2.create in
    h.(i) <- hi;
    s.(i) <- si
  done

open Int64

let rec binsearch ~ok left right =
  if abs (right - left) <= 1L then right
  else
    let mid = (right + left) / 2L in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans = binsearch 0L (1L lsl 60) ~ok:(fun x ->
    Iter.(0 -- Int.pred n)
    |> Fn.flip Iter.fold_while [] (fun acc i ->
        if x < h.(i) then [], `Stop
        else
          (x - h.(i)) / s.(i) ::acc , `Continue
      )
    |> List.sort ~compare
    |> fun t ->
    not (List.is_empty t) && List.for_alli t ~f:(fun i t -> of_int i <= t)
  )

let () = printf "%Ld\n%!" ans
