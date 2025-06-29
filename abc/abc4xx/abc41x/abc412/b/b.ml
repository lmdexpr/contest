open Core
open Scanf

let s = scanf " %s" Fn.id
let t = scanf " %s" String.to_list |> Char.Set.of_list

let n = String.length s

let yes =
  Iter.(2 -- pred n)
  |> Iter.for_all (fun i ->
    Char.is_lowercase s.[i] ||
    Set.mem t s.[i-1]
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
