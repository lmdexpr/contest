open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let s = Array.create ~len:(n + 1) 0L
let update v i = Int64.(v lxor s.(i))
let rec dfs d k v hs =
  Iter.(1 -- (k + 1)) |> Iter.fold (fun hs i ->
    invalid_arg "todo"
  ) hs

let ans = 
  Int64.Hash_set.create ()
  |> dfs 1 0 0L
  |> Hash_set.length

let () = printf "%d\n%!" ans
