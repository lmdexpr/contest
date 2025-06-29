open Core
open Scanf

let t = scanf " %d" Fn.id

let () =
  for _ = 1 to t do
    scanf " %d" @@ fun n ->
    let s0 = scanf " %d" Fn.id in
    let s  = Array.init (n-2) ~f:(fun _ -> scanf " %d" Fn.id) |> Int.Set.of_array in
    let sn = scanf " %d" Fn.id in

    let rec solve acc si s =
      if sn <= 2 * si then acc + 1
      else
        match Set.binary_search ~compare s `Last_less_than_or_equal_to (2 * si) with
        | None    -> -1
        | Some si -> solve (acc + 1) si @@ Set.remove s si
    in

    printf "%d\n%!" @@ solve 1 s0 s
  done
