open Core
open Scanf

let n = scanf "%d" Fn.id

let items = Array.create ~len:n (0, 0)
let seat = ref 0
let () =
  for i = 0 to n - 1 do
    let x, y, z = scanf " %d %d %d" Tuple3.create in
    seat := !seat + z;
    items.(i) <- max 0 ((x + y) / 2 + 1 - x), z
  done
let seat = !seat

let dp = Array.create ~len:(seat + 1) 1_000_000_000_000_000_000
let () =
  dp.(0) <- 0;
  Array.iter items ~f:(fun (w, z) ->
    for j = seat downto z do
      dp.(j) <- min dp.(j) (dp.(j - z) + w)
    done
  )

let ans =
  Iter.(seat / 2 + 1 -- seat)
  |> Iter.map (Array.get dp)
  |> Iter.min_exn ~lt:(<)

let () = printf "%d\n%!" ans
