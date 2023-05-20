open Core
open Scanf

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v
      
  let around g v = find g v |> Option.value ~default:Iter.empty
end

let iter_zip a b = Iter.flat_map (fun a -> Iter.map (fun b -> a, b) b) a

let solve n c g =
  let dist = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) @@ -1 in
  let rec bfs q =
    match Queue.dequeue q with
    | None        -> ()
    | Some (t, a) ->
      iter_zip (Graph.around g t) (Graph.around g a)
      |> Iter.filter (fun (t, a) -> dist.(t).(a) = -1 && c.(t) <> c.(a))
      |> Iter.iter (fun (dt, da) ->
          dist.(dt).(da) <- dist.(t).(a) + 1;
          Queue.enqueue q (dt, da)
        );
      bfs q
  in
  dist.(1).(n) <- 0; bfs @@ Queue.singleton (1, n);
  dist.(n).(1)

let t = scanf "%d" ident

let () =
  for _ = 1 to t do
    let n, m = scanf " %d %d" Tuple2.create in

    let c = Array.init (n + 1) ~f:(const 0) in
    for i = 1 to n do
      scanf " %d" (Array.set c i)
    done;

    let g = Graph.create ~size:n (module Int) in
    for _ = 1 to m do
      scanf " %d %d" @@ Graph.connect g
    done;

    let ans = solve n c g in

    printf "%d\n%!" ans
  done
