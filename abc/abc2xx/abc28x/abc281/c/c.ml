open Core

let n, t = Scanf.scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let sum = Array.sum (module Int) a ~f:ident

let () =
  Iter.of_array a
  |> Iter.zip_i
  |> Iter.fold_while
    (fun (_, t) (i, a) ->
      if t - a <= 0 then (i + 1, t), `Stop
      else
        (-1, t - a), `Continue
    )
    (-1, t % sum)
  |> Tuple2.uncurry (printf "%d %d\n%!")
