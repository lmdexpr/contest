open Core
open Scanf

let n1, n2, m = scanf "%d %d %d" Tuple3.create
module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:(n1+n2) (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.connect g
  done

let longest_path start =
  let queue  = Queue.singleton start in
  let path  = Array.create ~len:(n1+n2+1) (-1) in
  path.(start) <- 0;
  let rec loop acc =
    match Queue.dequeue queue with
    | None   -> acc
    | Some v ->
      Graph.around g v
      |> Iter.filter (fun u -> path.(u) < 0)
      |> Iter.iter   (fun u ->
        path.(u) <- path.(v) + 1;
        Queue.enqueue queue u
      );
      loop (max acc path.(v))
  in
  loop 0

let ans = longest_path 1 + longest_path (n1+n2) + 1

let () = printf "%d\n%!" ans
