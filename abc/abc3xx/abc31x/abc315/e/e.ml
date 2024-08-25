open Core
open Scanf

let n = scanf "%d" Fn.id
module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for i = 1 to n do
    let c = scanf " %d" Fn.id in
    for _ = 1 to c do
      let p = scanf " %d" Fn.id in
      Graph.push g i p
    done
  done

let dfs g init =
  let visited = Array.init (n + 1) ~f:(const false) in
  let rec backtrack acc v = 
    let acc =
      Graph.around g v
      |> Iter.filter (fun u -> not visited.(u))
      |> Iter.fold backtrack acc
    in
    visited.(v) <- true;
    Iter.snoc acc v
  in
  backtrack Iter.empty init
 
let () =
  dfs g 1
  |> Iter.take_while (fun v -> v <> 1)
  |> Iter.iter (printf "%d ");
  printf "\n"
