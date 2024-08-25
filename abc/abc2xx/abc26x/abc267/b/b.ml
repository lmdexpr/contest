open Core
open Scanf

let s = scanf "%s" ident |> String.to_array |> Array.map ~f:Char.((=) '1')

let lane = [| s.(6); s.(3); s.(7) || s.(1); s.(4); s.(8) || s.(2); s.(5); s.(9) |]

let is_split =
  not s.(0) &&
  Iter.(0 -- 6) |> Iter.flat_map (fun i ->
      Iter.(i + 1 -- 6) |> Iter.filter_map (fun j ->
          Option.some_if (lane.(i) && lane.(j)) Iter.((i + 1) -- (j - 1))
        )
    )
  |> Iter.exists (
    Iter.exists @@ fun k -> not lane.(k)
  )

let ans = if is_split then "Yes" else "No"
let () = printf "%s\n%!" ans
