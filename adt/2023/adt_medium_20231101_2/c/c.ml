module IntSet = Set.Make(Int)

open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let f a b = 4 * a * b + 3 * a + 3 * b

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let possibles =
  Iter.((1 -- 1000) * (1 -- 1000))
  |> Iter.map (fun (a, b) -> f a b)
  |> Iter.to_list
  |> IntSet.of_list

let ans = Array.count s ~f:(fun s -> not @@ IntSet.mem s possibles)

let () = printf "%d\n%!" ans
