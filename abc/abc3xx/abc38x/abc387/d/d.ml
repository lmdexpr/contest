open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let start = ref (0, 0)
let goal  = ref (0, 0)
let () = 
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      match s.(i).(j) with
      | 'S' -> start := (i, j)
      | 'G' -> goal  := (i, j)
      | _ -> ()
    done
  done
let sx, sy = !start
let gx, gy = !goal

let bfs c =
  let dist = Array.make_matrix ~dimx:h ~dimy:w Int.max_value in
  dist.(sx).(sy) <- 0;

  let d = [| 
    [| 1, 0; -1, 0 |];
    [| 0, 1; 0, -1 |];
  |] in

  let q = Queue.create () in
  Queue.enqueue q (sx, sy, c);

  let rec bfs () =
    match Queue.dequeue q with
    | None           -> ()
    | Some (x, y, c) ->
      Array.iter d.(c) ~f:(fun (dx, dy) ->
        let nx, ny = x + dx, y + dy in
        if 
          0 <= nx && nx < h && 0 <= ny && ny < w && 
          dist.(nx).(ny) = Int.max_value && 
          Char.(s.(nx).(ny) <> '#')
        then (
          dist.(nx).(ny) <- dist.(x).(y) + 1;
          Queue.enqueue q (nx, ny, (c + 1) % 2)
        )
      );
      bfs ()
  in
  bfs ();
  dist.(gx).(gy)

let ans = min (bfs 0) (bfs 1)
let ans = if ans = Int.max_value then -1 else ans

let () = printf "%d\n%!" ans
