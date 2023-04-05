open Core
open Scanf

let q, a, b = scanf "%d %d %d" Tuple3.create

let solve s a b =
  let l = Int.Set.binary_search s ~compare `First_greater_than_or_equal_to a in
  let r = Int.Set.binary_search s ~compare `Last_less_than_or_equal_to b in
  if Option.exists l ~f:(fun l -> l <= b) && Option.exists r ~f:(fun r -> a <= r) then 0
  else
    let l = Int.Set.binary_search s ~compare `Last_strictly_less_than a in
    let r = Int.Set.binary_search s ~compare `First_strictly_greater_than b in
    match l, r with
    | Some l, None   -> a - l
    | None,   Some r -> r - b
    | Some l, Some r -> Int.min (a - l) (r - b)
    | None,   None   -> 0

let (+:) = Int.Set.add
let () =
  Iter.(1 -- q) |> Iter.fold
    (fun s _ ->
       let t, a, b = scanf " %d %d %d" Tuple3.create in
       match t with
       | 1 -> s +: (a + b) +: (a - b)
       | _ -> solve s a b |> printf "%d\n%!"; s
    )
    (Int.Set.singleton (a + b) +: (a - b))
  |> ignore
