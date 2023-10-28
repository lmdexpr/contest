open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort a ~compare

let a = Array.append a [| Int.max_value |]

let rec solve ?(ans=0) l r =
  if l > n - 1 || r > n - 1 then ans
  else
    Iter.(r -- n)
    |> Iter.find_pred_exn (fun r -> a.(r) >= a.(l) + m)
    |> fun r ->
      solve ~ans:(max ans (r - l)) (l + 1) r

let ans = solve 0 0

let () = printf "%d\n%!" ans
