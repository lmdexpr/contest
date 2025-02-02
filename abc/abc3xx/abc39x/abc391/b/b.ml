open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)
let t = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let a, b =
  Iter.(product (0 -- (n - m)) (0 -- (n - m)))
  |> Iter.find_pred_exn (fun (a, b) ->
    Iter.(product (0 -- (m - 1)) (0 -- (m - 1)))
    |> Iter.for_all Char.(fun (i, j) ->
      s.(a + i).(b + j) = t.(i).(j)
    )
  )

let a, b = a + 1, b + 1

let () = printf "%d %d\n%!" a b
