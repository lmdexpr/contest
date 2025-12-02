open Core
open Scanf

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = Array.folding_mapi a ~init:0 ~f:(fun l r x ->
  let r =
    Iter.(r -- pred n)
    |> Iter.find_pred (fun r -> k < a.(r) - x)
    |> Option.value ~default:n
  in
  let r = pred r in
  r, r - l
)

let ans = Array.sum (module Int) ans ~f:Fn.id

let () = printf "%d\n%!" ans
