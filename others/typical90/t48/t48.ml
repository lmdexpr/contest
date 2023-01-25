open Core

let n, k = Scanf.scanf "%d %d" Tuple2.create

let scores =
  Iter.(1 -- n)
  |> Iter.flat_map (fun _ ->
      let a, b = Scanf.scanf " %d %d" Tuple2.create in
      Iter.doubleton (a - b) b
    )
  |> Iter.to_array
let () = Array.sort scores ~compare:(Fn.flip compare)

let () =
  Iter.(0 -- (k - 1))
  |> Iter.map (Array.get scores)
  |> Iter.sum
  |> printf "%d\n%!"
