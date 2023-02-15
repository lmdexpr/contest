open Core
open Scanf

let n = scanf "%d" ident
let p, q, r = scanf " %d %d %d" Tuple3.create

let sum =
  Iter.(1 -- n) |> Iter.map (fun _ -> scanf " %d" ident)
  |> Iter.scan (+) 0 |> Iter.fold Int.Set.add Int.Set.empty

let (.?{}) = Int.Set.mem

let yes = Int.Set.exists sum ~f:(fun x -> sum.?{p + x} && sum.?{q + p + x} && sum.?{r + q + p + x})
let yes = if yes then "Yes" else "No"

let () = printf "%s\n%!" yes
