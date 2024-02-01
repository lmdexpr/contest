open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)
let x1, y1, x2, y2 =
  let x1, y1, x2, y2 = ref (-1), ref (-1), ref (-1), ref (-1) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if Char.(s.(i).(j) = 'P') then begin
        if !x1 = -1 && !y1 = -1 then
          ( x1 := i; y1 := j )
        else
          ( x2 := i; y2 := j )
      end
    done
  done;
  !x1, !y1, !x2, !y2

let go i x y =
  let dx = [| 1; 0; -1; 0 |] and dy = [| 0; 1; 0; -1 |] in
  x + dx.(i), y + dy.(i)

let back_if_blocked (bx, by) (x, y) =
  let in_range  = 0 <= x && x < n && 0 <= y && y < n in 
  if in_range && Char.(s.(x).(y) <> '#') then x, y else bx, by

let dist =
  Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ ->
    Array.make_matrix ~dimx:n ~dimy:n Int.max_value
  ))

let () =
  dist.(x1).(y1).(x2).(y2) <- 0;
  let q = Queue.create () in
  Queue.enqueue q (x1, y1, x2, y2);
  while not @@ Queue.is_empty q do
    let x1, y1, x2, y2 = Queue.dequeue_exn q in
    Iter.(0 -- 3) |> Iter.iter (fun i ->
      let nx1, ny1 = go i x1 y1 |> back_if_blocked (x1, y1) in
      let nx2, ny2 = go i x2 y2 |> back_if_blocked (x2, y2) in
      if dist.(nx1).(ny1).(nx2).(ny2) = Int.max_value then begin
        dist.(nx1).(ny1).(nx2).(ny2) <- dist.(x1).(y1).(x2).(y2) + 1;
        Queue.enqueue q (nx1, ny1, nx2, ny2)
      end
    )
  done

let ans =
  Iter.(0 -- (n - 1)) |> Iter.flat_map (fun i ->
  Iter.(0 -- (n - 1)) |> Iter.map      (fun j -> i, j))
  |> Iter.fold (fun ans (i, j) -> min ans dist.(i).(j).(i).(j)) Int.max_value
let ans = if ans = Int.max_value then -1 else ans

let () = printf "%d\n%!" ans
