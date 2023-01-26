open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let map = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) (-1)

let sqrts = Array.init (m + 1) ~f:(const None)
let () =
  for i = 0 to m do
    if i * i <= m then
      sqrts.(i * i) <- Some i
  done

let rec bfs q =
  match Queue.dequeue q with
  | None        -> ()
  | Some (i, j) ->
    let now = map.(i).(j) in
    Iter.(1 -- n)
    |> Iter.flat_map (fun k ->
        let x = m - (i - k) * (i - k) in
        if 0 <= x && x <= m then
          match sqrts.(x) with
          | None   -> Iter.empty
          | Some x -> Iter.doubleton (k, j + x) (k, j - x)
        else
          Iter.empty
      )
    |> Iter.filter (fun (k, l) -> 1 <= k && k <= n && 1 <= l && l <= n)
    |> Iter.iter (fun (k, l) ->
        if map.(k).(l) = -1 then begin
          map.(k).(l) <- now + 1;
          Queue.enqueue q (k, l)
        end
      );
    bfs q

let () =
  map.(1).(1) <- 0;
  bfs @@ Queue.singleton (1, 1);
  for i = 1 to n do
    for j = 1 to n do
      printf "%d " map.(i).(j)
    done;
    printf "\n%!"
  done
