open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) 'o'))

let row = Array.init n ~f:(fun i -> Array.count s.(i) ~f:Fn.id)
let col = Array.init n ~f:(fun j -> Array.count s ~f:(fun row -> row.(j)))

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let ans = 
  Iter.(
    (0 -- (n - 1)) * (0 -- (n - 1))
  )
  |> Iter.fold (fun acc (i, j) -> 
    acc + if s.(i).(j) then (row.(i) - 1) * (col.(j) - 1) else 0
  ) 0

let () = printf "%d\n%!" ans
