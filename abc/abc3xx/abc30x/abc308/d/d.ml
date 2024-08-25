open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let bfs () =
  let sx = 0 and sy = 0 in
  let snuke i = "snuke".[i % 5] in
  let visited = Array.make_matrix ~dimx:h ~dimy:w false in visited.(sy).(sx) <- true;
  let rec bfs q = 
    match Fqueue.dequeue q with
    | None                                            -> false
    | Some ((_, x, y), _) when x = w - 1 && y = h - 1 -> true
    | Some ((d, x, y), q) ->
      let d = d + 1 in
      Iter.of_list [ (x+1, y); (x-1, y); (x, y+1); (x, y-1) ]
      |> Iter.filter (fun (x, y) ->
          0 <= x && x < w && 0 <= y && y < h &&
          not visited.(y).(x) &&
          Char.(s.(y).(x) = snuke d)
        )
      |> Fn.flip Iter.fold q (fun q (x, y) ->
          visited.(y).(x) <- true;
          Fqueue.enqueue q (d, x, y)
        )
      |> bfs
  in
  bfs @@ Fqueue.singleton (0, sx, sy)

let yes = bfs ()

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
