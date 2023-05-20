open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.connect g
  done

let color = Array.init (n+1) ~f:(const 1)

let dist = Array.init (n+1) ~f:(fun _ -> Array.init (n+1) ~f:(const @@ -1))

let bfs p d =
  let rec bfs q = 
    match Fqueue.dequeue q with
    | None                           -> ()
    | Some ((di, v), q) when di >= d ->
      if dist.(p).(v) < 0 then dist.(p).(v) <- di; bfs q
    | Some ((di, v), q) ->
      if dist.(p).(v) < 0 then dist.(p).(v) <- di;
      color.(v) <- 0;
      Graph.around g v
      |> Iter.filter (fun u -> dist.(p).(u) = -1)
      |> Iter.fold   (fun q u -> Fqueue.enqueue q (di + 1, u)) q
      |> bfs
  in
  bfs @@ Fqueue.singleton (0, p)

let k  = scanf " %d" ident
let pd = Array.init k ~f:(fun _ -> scanf " %d %d" Tuple2.create) |> Iter.of_array

let () = Iter.iter (Tuple2.uncurry bfs) pd

let satisfied =
  Array.exists color ~f:((=) 1) &&
  pd |> Iter.for_all (fun (p, d) ->
      let iter = Iter.(1 -- n) |> Iter.filter (fun v -> dist.(p).(v) <> -1) in
      iter |> Iter.filter (fun v -> dist.(p).(v) < d && color.(v) <> 0) |> Iter.is_empty &&
      iter |> Iter.filter (fun v -> dist.(p).(v) = d && color.(v) = 1)  |> Iter.is_empty |> not
    )

let () =
  if not satisfied then printf "No\n%!"
  else begin
    printf "Yes\n%!";
    for i = 1 to n do
      printf "%d" color.(i)
    done;
    printf "\n%!"
  end
