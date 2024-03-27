open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  Array.fold a ~init:(0, []) ~f:(fun (sum, acc) a ->
    let sum = sum + 1 in
    let sum, acc =
      match acc with
      | []                      -> sum, [a, 1]
      | (x, _) :: _ when x <> a -> sum, (a, 1) :: acc
      | (_, c) :: rest          ->
        if c + 1 = a then sum - a, rest
        else
          sum, (a, c + 1) :: rest
    in
    printf "%d\n" sum;
    sum, acc
  )
  |> ignore
