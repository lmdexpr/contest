open Core
open Scanf

let a = Array.init 7 ~f:(fun _ -> scanf " %d" Fn.id)

let yes =
  Iter.(product (1 -- 13) (1 -- 13))
  |> Iter.filter (fun (x, y) -> x <> y)
  |> Iter.exists (fun (x, y) ->
    Array.count a ~f:((=) x) >= 2 &&
    Array.count a ~f:((=) y) >= 3
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
