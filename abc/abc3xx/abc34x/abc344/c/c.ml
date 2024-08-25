open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let m = scanf " %d" Fn.id
let b = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let l = scanf " %d" Fn.id
let c = Array.init l ~f:(fun _ -> scanf " %d" Fn.id)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let set =
  Iter.((0 -- (n - 1)) * (0 -- (m - 1)) * (0 -- (l - 1)))
  |> Iter.fold (fun set ((i, j), k) -> 
    Set.add set (a.(i) + b.(j) + c.(k))
  ) Int.Set.empty

let () = 
  let q = scanf " %d" Fn.id in
  for _ = 1 to q do
    let d = scanf " %d" Fn.id in
    printf "%s\n%!" @@
    if Set.mem set d then "Yes" else "No"
  done
