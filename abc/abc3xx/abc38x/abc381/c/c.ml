open Core
open Scanf

let n = scanf " %d" Fn.id
let s = scanf " %s" Fn.id

let solve i =
  Iter.(0 -- n)
  |> Iter.find_pred (fun d ->
    let d = d + 1 in
    let l = i - d and r = i + d in
    not (
      0 <= l && r < n &&
      Char.(s.[l] = '1' && s.[r] = '2')
    )
  )
  |> Option.value_map ~default:1 ~f:(fun d -> 2 * d + 1)

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.fold
    (fun ans i ->
      match s.[i] with
      | '/' -> max ans @@ solve i
      | _   -> ans
    )
    0

let () =
  printf "%d\n%!" ans
