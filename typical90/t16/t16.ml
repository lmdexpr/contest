open Core

let n = Scanf.scanf "%d" ident
let a, b, c = Scanf.scanf " %d %d %d" Tuple3.create

let () =
  Iter.(0 -- (n / a + 1))
  |> Iter.flat_map (fun x ->
      Iter.(0 -- (9999 - x))
      |> Iter.filter_map (fun y ->
          let change = n - a * x - b * y in
          let r = x + y + change / c in
          Option.some_if (change >= 0 && change % c = 0 && r <= 9999) r
        )
    )
  |> Iter.min_exn ~lt:(<)
  |> printf "%d\n%!"
