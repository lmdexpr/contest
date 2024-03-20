open Core
open Scanf

let n = scanf "%d" Fn.id

let xy = Array.init n ~f:(fun _ ->
  scanf " %d %d" @@ fun x y -> x + 1005, y + 1005
)

let grid = Array.make_matrix ~dimx:2010 ~dimy:2010 false
let () = Array.iter xy ~f:(fun (x, y) -> grid.(x).(y) <- true)

let uf = 
  Array.init 2010 ~f:(fun i ->
  Array.init 2010 ~f:(fun j -> Union_find.create (i * 3000 + j)))

let () =
  Array.iter xy ~f:(fun (x, y) -> 
    [ (-1, 0); (0, -1); (-1, -1); (1, 0); (0, 1); (1, 1) ]
    |> List.iter ~f:(fun (dx, dy) ->
      if grid.(x + dx).(y + dy) then
        Union_find.union uf.(x).(y) uf.(x + dx).(y + dy)
    )
  )

let ans =
  Array.fold xy ~init:Int.Set.empty ~f:(fun acc (x, y) ->
    Set.add acc (Union_find.get uf.(x).(y))
  )
  |> Set.length

let () = printf "%d\n%!" ans
