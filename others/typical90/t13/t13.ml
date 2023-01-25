open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

module Graph = struct 
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)

  let get g v = find g v |> Option.value ~default:[]
end

let g = Graph.create ~size:400_001 (module Int)

let () =
  for _ = 1 to m do
    let a, b, c = Scanf.scanf " %d %d %d" Tuple3.create in
    Graph.push g a (b, c);
    Graph.push g b (a, c)
  done

let rec dijkstra dist q = 
  match Queue.dequeue q with
  | None -> ()
  | Some v ->
    let f (u , cost) =
      if dist.(u) > dist.(v) + cost then begin
        dist.(u) <- dist.(v) + cost;
        Queue.enqueue q u
      end
    in
    List.iter ~f @@ Graph.get g v;
    dijkstra dist q

let inf = 1000_000_000

let dist_from_1 = Array.init (n+1) ~f:(const inf)
let () =
  dist_from_1.(1) <- 0;
  dijkstra dist_from_1 (Queue.singleton 1)

let dist_from_n = Array.init (n+1) ~f:(const inf)
let () =
  dist_from_n.(n) <- 0;
  dijkstra dist_from_n (Queue.singleton n)

let () =
  for i = 1 to n do
    printf "%d\n%!" @@ dist_from_1.(i) + dist_from_n.(i)
  done
