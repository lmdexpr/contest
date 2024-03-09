open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

let s = Array.init n ~f:(fun _ -> Scanf.scanf " %s" ident)

let () =
  Iter.(0 -- (n - 2)) |> Iter.flat_map (fun a ->
      Iter.((a + 1) -- (n - 1)) |> Iter.filter (fun b ->
          Iter.(0 -- (m - 1))
          |> Iter.for_all (fun i ->Char.(s.(a).[i] = 'o' || s.(b).[i] = 'o'))
        )
    )
  |> Iter.length
  |> printf "%d\n%!"
