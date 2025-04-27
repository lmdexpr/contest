open Core
open Scanf

let n = scanf " %d" Fn.id
let _m = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let priv = Array.create ~len:(n + 1) Int.Set.empty
let priv_all = Array.create ~len:(n + 1) false

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      scanf " %d %d" @@ fun x y -> priv.(x) <- Set.add priv.(x) y
    | 2 ->
      scanf " %d" @@ fun x -> priv_all.(x) <- true
    | _ ->
      scanf " %d %d" @@ fun x y ->
      printf "%s\n" @@
      if priv_all.(x) || Set.mem priv.(x) y then "Yes" else "No"
  done
