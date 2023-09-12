open Core
open Scanf

let _n = scanf "%d" ident
let a, b = scanf " %d %d" @@ fun a b -> a - 1, b - 1
let c, d  = scanf " %d %d" @@ fun c d -> c - 1, d - 1

let s = scanf " %s" String.to_array |> Array.map ~f:(function '.' -> true | _ -> false)

let yes =
  let stop = max c d in
  Iter.(a -- (stop - 2)) |> Iter.for_all (fun i -> s.(i) || s.(i + 1))
  && (
    c < d ||
    Iter.(b -- d) |> Iter.exists (fun i -> s.(i - 1) && s.(i) && s.(i + 1))
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
