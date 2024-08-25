open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let a = Array.init h ~f:(fun _ -> scanf " %s" ident)
let b = Array.init h ~f:(fun _ -> scanf " %s" ident)

let yes =
  Iter.(0 -- (h - 1)) |> Iter.flat_map (fun s ->
      Iter.(0 -- (w - 1)) |> Iter.map (fun t -> s, t)
    )
  |> Iter.exists (fun (s, t) ->
      Iter.(0 -- (h - 1)) |> Iter.flat_map (fun i ->
          Iter.(0 -- (w - 1)) |> Iter.map (fun j -> i, j)
        )
      |> Iter.for_all (fun (i, j) ->
          let a = a.((i + s) % h).[(j + t) % w] in
          let b = b.(i).[j] in
          Char.(a = b)
        )
    )

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
