open Core

let s = In_channel.(input_lines stdin) |> Array.of_list

let inf = 1_000_000_000

let a, b, c, d =
  Iter.(0 -- 9) |> Iter.flat_map (fun i ->
      Iter.(0 -- 9) |> Iter.filter_map (fun j -> Option.some_if Char.(s.(i).[j] = '#') (i + 1, j + 1))
    )
  |> Iter.fold
    (fun (a, b, c, d) (i, j) -> min a i, max b i, min c j, max d j)
    (inf, -inf, inf, -inf)

let () = printf "%d %d\n%d %d\n%!" a b c d
