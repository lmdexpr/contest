open Core
open Scanf

let r = scanf " %d" Fn.id

let check a b =
  (2 * a + 1) * (2 * a + 1) + (2 * b + 1) * (2 * b + 1) <= 4 * r * r

let ans, _ =
  Iter.iterate ((+) 1) 1
  |> Iter.take_while (fun x -> check x 1)
  |> Iter.fold (fun (cnt, up) x ->
    let up =
      Iter.(up --^ 0)
      |> Iter.find_pred_exn (fun up -> check x up)
    in
    (cnt + up, up)
  ) (0, r - 1)

let ans = (r - 1) * 4 + 1 + ans * 4

let () = printf "%d\n%!" ans
