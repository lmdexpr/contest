open Core
open Scanf

let n = scanf " %d" Fn.id
let d = Array.init (n-1) ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.append [| init |] @@ Array.folding_map a ~init ~f:(paired f)

let cumsum = cumsum ~init:0 ~f:Int.(+) d

let () =
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      printf "%d %!" (cumsum.(j) - cumsum.(i))
    done;
    printf "\n%!"
  done
