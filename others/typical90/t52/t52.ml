open Core

let n = Scanf.scanf "%d" ident

let m = 1000000007

let () =
  Iter.(1 -- n)
  |> Iter.map (fun _ ->
      Iter.(1 -- 6)
      |> Iter.map (fun _ -> Scanf.scanf " %d" ident)
      |> Iter.sum
    )
  |> Iter.fold (fun acc p -> acc * p % m) 1
  |> printf "%d\n%!"
