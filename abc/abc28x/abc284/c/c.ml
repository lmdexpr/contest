open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.snoc acc v
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let next g v = find g v |> Option.value ~default:Iter.empty
end

let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    Scanf.scanf " %d %d" @@ Graph.connect g
  done

let dist = Array.init (n+1) ~f:(const @@ -1)
let bfs g init =
  dist.(init) <- 0;
  let q = Queue.singleton init in
  let rec bfs () =
    match Queue.dequeue q with
    | None -> ()
    | Some v ->
      Graph.next g v
      |> Iter.iter (fun u ->
          if dist.(u) = -1 then ( dist.(u) <- dist.(v) + 1; Queue.enqueue q u )
        );
      bfs ()
  in
  bfs ()

let skip = 
  Iter.(1 -- n)
  |> Iter.fold
    (fun skip start ->
       if dist.(start) <> -1 then skip + 1
       else
         (bfs g start; skip)
    )
    0

let () = printf "%d\n%!" (n - skip)
