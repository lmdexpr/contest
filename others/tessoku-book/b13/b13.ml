open Core
open Scanf

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.append [| init |] @@ Array.folding_map a ~init ~f:(paired f)

let s = cumsum ~init:0 ~f:(+) a
let s l r = s.(r + 1) - s.(l)

let ans = 
  Iter.(0 -- pred n)
  |> IterLabels.fold_map ~init:0 ~f:(fun r l ->
    let r =
      Iter.(r -- pred n)
      |> Iter.find_pred (fun r -> k < s l r)
      |> Option.value ~default:n
    in
    r, r - l
  )
  |> Iter.sum

let () = printf "%d\n%!" ans
