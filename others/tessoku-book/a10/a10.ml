open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let d = scanf " %d" Fn.id

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init ~f:(paired f)

let p = cumsum ~init:a.(0)   ~f:Int.max a
let q = cumsum ~init:a.(n-1) ~f:Int.max Array.(rev a) |> Array.rev

let () =
  for _ = 1 to d do
    scanf " %d %d" @@ fun l r ->
    printf "%d\n" @@ Int.max p.(l - 2) q.(r)
  done
