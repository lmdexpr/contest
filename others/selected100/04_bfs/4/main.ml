(* https://atcoder.jp/contests/joi2012yo/tasks/joi2012yo_e *)
open Core
open Scanf

let w, h = scanf "%d %d" Tuple2.create
let is_empty = Array.init (h+2) ~f:(fun _ -> Array.init (w+2) ~f:(const true))
let () =
  for i = 1 to h do
    for j = 1 to w do
      scanf " %d" (fun b -> is_empty.(i).(j) <- b <> 1)
    done
  done

let is_empty (i, j) = is_empty.(i).(j)

let inside (i, j) = 0 <= i && i <= h+1 && 0 <= j && j <= w+1

let hexagon_neighbor (i, j) =
  if i % 2 <> 0
  then
    Iter.of_list [ (i-1, j); (i-1, j+1); (i, j-1); (i, j+1); (i+1, j); (i+1, j+1) ]
  else
    Iter.of_list [ (i-1, j-1); (i-1, j); (i, j-1); (i, j+1); (i+1, j-1); (i+1, j) ]

let ans =
  let sx, sy = 0, 0 in
  let dst = Array.init (h+2) ~f:(fun _ -> Array.create ~len:(w+2) @@ -1) in dst.(sy).(sx) <- 0;
  let nbd pos = hexagon_neighbor pos |> Iter.filter inside |> Iter.filter (fun (i, j) -> dst.(i).(j) = -1) in 
  let rec bfs acc q = 
    match Fqueue.dequeue q with
    | None                -> acc
    | Some ((d, pos), q) ->
      let around = nbd pos in
      let empty  = Iter.filter is_empty around in
      let acc    = acc + Iter.length around - Iter.length empty in
      let d      = d + 1 in
      empty
      |> Fn.flip Iter.fold q (fun q (i, j as pos) ->
          if dst.(i).(j) = -1 then dst.(i).(j) <- d;
          Fqueue.enqueue q (d, pos)
        )
      |> bfs acc
  in
  bfs 0 @@ Fqueue.singleton (0, (sy, sx))

let () = printf "%d\n%!" ans
