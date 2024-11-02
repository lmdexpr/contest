open Core
open Scanf

let h, w, k = scanf "%d %d %d" Tuple3.create

let s = Array.init h ~f:(fun _ -> 
  scanf " %s" String.to_array |> Array.map ~f:(function '#' -> 0 | _ -> Int.max_value)
)

let rec backtrack acc i x y =
  if i = k + 1 then (
    let acc = acc + Bool.to_int (i < s.(y).(x)) in
    s.(y).(x) <- Int.max_value;
    acc
  ) else (
    s.(y).(x) <- i;
    let acc = 
      [ 0, 1; 0, -1; 1, 0; -1, 0 ]
      |> List.fold ~init:acc ~f:(fun acc (dx, dy) ->
        let x = x + dx in let y = y + dy in
        if 0 <= x && x < w && 0 <= y && y < h && i < s.(y).(x) then
          backtrack acc (i + 1) x y
        else
          acc
      )
    in
    s.(y).(x) <- Int.max_value;
    acc
  )

let ans =
  Iter.product Iter.(0 -- (h - 1)) Iter.(0 -- (w - 1))
  |> Iter.filter (fun (y, x) -> s.(y).(x) = Int.max_value)
  |> Iter.fold (fun acc (y, x) -> acc + backtrack 0 1 x y) 0

let () = printf "%d\n%!" ans
