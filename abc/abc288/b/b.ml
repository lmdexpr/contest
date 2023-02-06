open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let s = Iter.(1 -- n) |> Iter.map (fun _ -> scanf " %s" ident) |> Iter.take k |> Iter.to_array

let () =
  Array.sort s ~compare:String.compare;
  Array.iter s ~f:(printf "%s\n%!")
