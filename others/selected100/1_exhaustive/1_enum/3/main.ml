(* https://atcoder.jp/contests/abc122/tasks/abc122_b *)

open Core
open Scanf

let s = scanf "%s" ident
let n = String.length s

let ans =
  Iter.(0 -- (n - 1)) |> Iter.flat_map (fun s ->
      Iter.(1 -- (n - s))
      |> Iter.map (fun e -> s, e)
    )
  |> Iter.map (fun (i, len) -> String.slice s i (i + len))
  |> Iter.filter (fun s ->
      String.find s ~f:(function
          | 'A' | 'C' | 'G' | 'T' -> false
          | _ -> true
        )
      |> Option.is_none
    )
  |> Iter.map String.length
  |> Iter.max ~lt:(<)
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
