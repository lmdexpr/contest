open Core
open Scanf

let n = scanf "%d" ident

module WeightedGraph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u c = update g v ~f:(push (u, c))
  let connect g v u c = push g v u c; push g u v c

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = WeightedGraph.create ~size:n (module Int)

let () = 
  for _ = 1 to n - 1 do
    scanf " %d %d %d" (WeightedGraph.connect g)
  done

let () =
  let visited = Array.init (n+1) ~f:(const false) in
  visited.(1) <- true;

  let color = Array.init (n+1) ~f:(const @@ -1) in
  color.(1) <- 0;

  let q = Queue.singleton 1 in

  let rec bfs () =
    match Queue.dequeue q with
    | None   -> ()
    | Some v ->
      WeightedGraph.around g v
      |> Iter.filter (fun (u, _) -> not visited.(u))
      |> Iter.iter (fun (u, w) ->
          let c = (color.(v) + Bool.to_int (w % 2 <> 0)) % 2 in
          color.(u)   <- c;
          visited.(u) <- true;
          Queue.enqueue q u
        );
      bfs ()
  in
  bfs ();

  for i = 1 to n do
    printf "%d\n" color.(i)
  done
