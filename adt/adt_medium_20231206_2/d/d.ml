open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let rec go acc x y dx dy = function
  | _ :: _ when x < 0 || w <= x || y < 0 || h <= y -> None
  | c :: rest when Char.(s.(y).(x) = c) ->
    go ((x + 1, y +1) :: acc) (x + dx) (y + dy) dx dy rest
  | _ :: _ -> None
  | []     -> Some acc

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let ans =
  Iter.((0 -- (h-1)) * (0 -- (w-1)))
  |> Iter.find_map (fun (y, x) ->
    let sunuke = ['s'; 'n'; 'u'; 'k'; 'e'] in
    let dirs = [(1, 0); (0, 1); (-1, 0); (0, -1); (1, 1); (-1, -1); (-1, 1); (1, -1)] in
    List.find_map dirs ~f:(fun (dx, dy) -> go [] x y dx dy sunuke)
  )
  |> Option.value ~default:[]

let () =
  List.rev ans
  |> List.iter ~f:(fun (y, x) -> printf "%d %d\n" x y);
