(* https://atcoder.jp/contests/abc007/tasks/abc007_3 *)
open Core
open Scanf

let r, c = scanf "%d %d" Tuple2.create

let sy, sx = scanf " %d %d" @@ fun y x -> y - 1, x - 1
let gy, gx = scanf " %d %d" @@ fun y x -> y - 1, x - 1

let map = Array.init r ~f:(fun _ -> scanf " %s" String.to_array)

let bfs () =
  let dist = Array.init r ~f:(fun _ -> Array.create ~len:c (-1)) in dist.(sy).(sx) <- 0;
  let rec bfs q = 
    match Fqueue.dequeue q with
    | None                                      -> -1
    | Some ((d, x, y), _) when x = gx && y = gy -> d
    | Some ((d, x, y), q) ->
      Iter.of_list [ (x+1, y); (x-1, y); (x, y+1); (x, y-1) ]
      |> Iter.filter (fun (x, y) -> Char.(map.(y).(x) = '.') && dist.(y).(x) = -1)
      |> Fn.flip Iter.fold q (fun q (x, y) ->
          if dist.(y).(x) = -1 then dist.(y).(x) <- d + 1;
          Fqueue.enqueue q (d + 1, x, y)
        )
      |> bfs
  in
  bfs @@ Fqueue.singleton (0, sx, sy)

let ans = bfs ()

let () = printf "%d\n%!" ans
