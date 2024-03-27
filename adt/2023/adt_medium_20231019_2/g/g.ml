open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)

  let around g v = find g v |> Option.value ~default:Iter.empty

  let topological_sort n g in_degree =
    let enqueue q i =
      if in_degree.(i) = 0 then Queue.enqueue q i
    in
    let q = Queue.create () in Iter.(1 -- n) |> Iter.iter (enqueue q);

    let rec loop len acc =
      match Queue.dequeue q with
      | None   -> Option.some_if (len = n) acc
      | Some v ->
        around g v |> Iter.iter (fun u -> in_degree.(u) <- in_degree.(u) - 1; enqueue q u);
        loop (len + 1) (Iter.snoc acc v)
    in
    loop 0 Iter.empty
end
let g = Graph.create ~size:n (module Int)
let in_degree = Array.create ~len:(n+1) 0
let () =
  for _ = 1 to m do
    let k = scanf " %d" Fn.id in
    let a = Array.init k ~f:(fun _ -> scanf " %d" Fn.id) in
    for i = 0 to k - 2 do
      Graph.push g a.(i) a.(i + 1);
      in_degree.(a.(i + 1)) <- in_degree.(a.(i + 1)) + 1
    done
  done

let yes = Graph.topological_sort n g in_degree |> Option.is_some

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
