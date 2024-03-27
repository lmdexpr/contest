open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) '#'))

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let white_on_6 (dx, dy) (x, y) =
  let route =
    Iter.(0 -- 5)
    |> Iter.map    (fun i -> x + dx * i, y + dy * i)
    |> Iter.filter (fun (x, y) -> 0 <= x && x < n && 0 <= y && y < n)
  in
  if Iter.length route <> 6 then 0
  else
    route
    |> Iter.map (fun (x, y) -> s.(x).(y))
    |> Iter.filter_count Fn.id

let yes = 
  Iter.(
    (0 -- (n - 1)) * (0 -- (n - 1))
  )
  |> Iter.find_pred (fun pos ->
    white_on_6 ( 1, 0) pos >= 4 ||
    white_on_6 (-1, 0) pos >= 4 ||
    white_on_6 ( 0, 1) pos >= 4 ||
    white_on_6 ( 0,-1) pos >= 4 ||
    white_on_6 ( 1, 1) pos >= 4 ||
    white_on_6 ( 1,-1) pos >= 4 ||
    white_on_6 (-1, 1) pos >= 4 ||
    white_on_6 (-1,-1) pos >= 4 
  )
  |> Option.is_some

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
