open Core
open Scanf

let x = scanf "%Ld" ident

open Int64

let () =
  Iter.(0 -- 59)
  |> Iter.filter (fun d -> x land (1L lsl d) <> 0L)
  |> Iter.fold
    (fun acc d -> Iter.append acc @@ Iter.map (fun r -> r lor (1L lsl d)) acc)
    (Iter.singleton 0L)
  |> Iter.iter (printf "%Ld\n%!")
