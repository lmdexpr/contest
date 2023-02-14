open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let a = Array.init h ~f:(fun _ -> scanf " %s" ident)

let init =
  Array.foldi a ~init:[] ~f:(fun i acc ->
      String.foldi ~init:acc ~f:(fun j acc a ->
          if Char.(a = '#') then (i, j) :: acc else acc
        )
    )

let count = Array.make_matrix ~dimx:h ~dimy:w @@ -1
let () =
  List.iter init ~f:(fun (i, j) -> count.(i).(j) <- 0)

let rec bfs q acc =
  match Queue.dequeue q with
  | None        -> acc - 1
  | Some (i, j) ->
    let now = count.(i).(j) + 1 in
    Iter.of_list [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
    |> Iter.map (fun (di, dj) -> i + di, j + dj)
    |> Iter.filter (fun (i, j) -> 0 <= i && i < h && 0 <= j && j < w)
    |> Iter.filter (fun (i, j) -> count.(i).(j) = -1)
    |> Iter.iter (fun (i, j) ->
        count.(i).(j) <- now;
        Queue.enqueue q (i, j)
      );
    bfs q (max acc now)

let ans = bfs (Queue.of_list init) @@ -1

let () = printf "%d\n%!" ans
