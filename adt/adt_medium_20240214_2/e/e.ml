open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) '#'))

let iter =
  Iter.(0 -- (h - 1))
  |> Iter.flat_map (fun r -> Iter.(0 -- (w - 1)) |> Iter.map (fun c -> r, c))

let uf = Array.init h ~f:(fun r -> Array.init w ~f:(fun c -> Union_find.create (r * w + c)))
let () =
  iter
  |> Iter.iter (fun (r, c) ->
    let d = [(0, 1); (1, 0); (0, -1); (-1, 0); (1, 1); (1, -1); (-1, 1); (-1, -1)] in
    List.iter d ~f:(fun (dr, dc) ->
      let nr, nc = r + dr, c + dc in
      if 0 <= nr && nr < h && 0 <= nc && nc < w && s.(r).(c) && s.(nr).(nc) then
        Union_find.union uf.(r).(c) uf.(nr).(nc)
    )
  )

let ans =
  iter
  |> Iter.filter_map (fun (r, c) -> Option.some_if s.(r).(c) @@ Union_find.get uf.(r).(c))
  |> Iter.to_list
  |> Int.Set.of_list
  |> Set.length

let () = printf "%d\n%!" ans
