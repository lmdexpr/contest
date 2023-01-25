open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

let lines = Array.init m ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)

let () =
  Array.foldi lines ~init:0 ~f:(fun i ans (ls, rs) ->
      Iter.(i + 1 -- (m - 1))
      |> Iter.filter (fun j ->
          let (lt, rt) = lines.(j) in
          (ls < lt && lt < rs && rs < rt) ||
          (lt < ls && ls < rt && rt < rs)
        )
      |> Iter.length
      |> fun c -> ans + c
    )
  |> printf "%d\n%!"
