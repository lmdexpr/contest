open Core
open Scanf

let h, w, x = scanf "%d %d %Ld" Tuple3.create
let p, q    = scanf " %d %d" Tuple2.create

let p, q = p - 1, q - 1

let s = 
  Array.init h ~f:(fun _ -> 
  Array.init w ~f:(fun _ -> scanf " %Ld" Fn.id))

module Heap = struct
  include Batteries.Heap
  let pop_min heap =
    if size heap = 0 then None
    else
      Some (find_min heap, del_min heap)
end

let (/^) x y = Int64.(x / y - if x % y = 0L then 1L else 0L)

let ans =
  let eaten = Array.make_matrix ~dimx:h ~dimy:w false in
  let rec bfs acc heap =
    match Heap.pop_min heap with
    | None                                      -> acc
    | Some ((_, p, q), heap) when eaten.(p).(q) -> bfs acc heap
    | Some ((r, p, q), heap) ->
      if Int64.(acc /^ x < r) then acc
      else
        let acc = Int64.(acc + r) in
        eaten.(p).(q) <- true;
        [ 1, 0; -1, 0; 0, 1; 0, -1 ]
        |> List.map ~f:(fun (dp, dq) -> p + dp, q + dq)
        |> List.filter ~f:(fun (p, q) -> 
          0 <= p && p < h && 0 <= q && q < w && not eaten.(p).(q)
        )
        |> List.fold ~init:heap ~f:(fun heap (p, q) -> Heap.add (s.(p).(q), p, q) heap)
        |> bfs acc
  in
  eaten.(p).(q) <- true;
  [ 1, 0; -1, 0; 0, 1; 0, -1 ]
  |> List.map ~f:(fun (dp, dq) -> p + dp, q + dq)
  |> List.filter ~f:(fun (p, q) -> 0 <= p && p < h && 0 <= q && q < w )
  |> List.fold ~init:Heap.empty ~f:(fun heap (p, q) -> Heap.add (s.(p).(q), p, q) heap)
  |> bfs s.(p).(q) 

let () = printf "%Ld\n%!" ans
