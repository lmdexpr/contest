open Core
open Scanf

let a, b, c, d = scanf "%Ld %Ld %Ld %Ld" Int64.(fun a b c d ->
  let adj = 1_000_000_000L in
  a + adj, b + adj, c + adj, d + adj
)

let mass = [| [| 2L; 1L; 0L; 1L |]; [| 1L; 2L; 1L; 0L |] |]

let ans =
  Iter.(0 -- 1) |> Iter.flat_map (fun fy ->
    Iter.(0 -- 3) |> Iter.map Int64.(fun fx ->
      let x = (c - of_int fx + 3L) / 4L - (a - of_int fx + 3L) / 4L in
      let y = (d - of_int fy + 1L) / 2L - (b - of_int fy + 1L) / 2L in
      x * y * mass.(fy).(fx)
    )
  )
  |> Iter.fold Int64.(+) 0L

let () = printf "%Ld\n%!" ans
