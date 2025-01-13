open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum = 
  let cumsum a =
    let paired f a b = let r = f a b in r, r in
    Array.folding_map a ~init:0 ~f:(paired Int.(+))
  in
  Array.append [| 0 |] @@ cumsum a

let () =
  for k = 0 to n - 1 do
    Iter.(0 -- (n - k - 1)) 
    |> Iter.map (fun i -> cumsum.(i + k + 1) - cumsum.(i)) 
    |> Iter.max_exn ~lt:(<)
    |> printf "%d\n"
  done
