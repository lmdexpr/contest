open Core

let n = Scanf.scanf "%d" ident

module Graph = struct 
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)

  let get g v = find g v |> Option.value ~default:[]
end

let g = Graph.create ~size:n (module Int)

let () =
  for _ = 1 to n - 1 do
    let a, b = Scanf.scanf " %d %d" Tuple2.create in
    Graph.push g a b;
    Graph.push g b a
  done

let bfs_max g n init =
  let dist = Array.init (n+1) ~f:(const @@ -1) in
  dist.(init) <- 0;
  let q = Queue.singleton init in
  let rec bfs () = 
    match Queue.dequeue q with
    | None -> ()
    | Some v ->
      Graph.get g v
      |> List.iter ~f:(fun u ->
          if dist.(u) = -1 then begin
            dist.(u) <- dist.(v) + 1;
            Queue.enqueue q u
          end);
      bfs ()
  in
  bfs ();
  Array.foldi dist ~init:(0, dist.(0)) ~f:(fun i (max_i, max_e) e ->
      if max_e < e then (i, e) else (max_i, max_e)
    )

let diameter =
  bfs_max g n 1
  |> Tuple2.get1
  |> bfs_max g n
  |> Tuple2.get2

let () = printf "%d\n%!" @@ diameter + 1
