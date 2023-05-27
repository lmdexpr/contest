open Core
open Scanf

let n = scanf "%d" ident
module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let cut u = function
    | None    -> Iter.empty
    | Some vs -> Iter.filter ((<>) u) vs
  let cut g v u = update g v ~f:(cut u)

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g   = Graph.create ~size:n (module Int)
let deg = Array.create ~len:(n+1) 0
let () =
  for _ = 1 to n - 1 do
    let u, v = scanf " %d %d" Tuple2.create in
    Graph.connect g u v;
    deg.(u) <- deg.(u) + 1;
    deg.(v) <- deg.(v) + 1
  done

let visited = Array.create ~len:(n+1) false
let rec solve ?(acc=[]) = function
  | []                        -> acc
  | v :: vs when deg.(v) <> 1 -> solve ~acc vs
  | v :: vs when visited.(v)  -> solve ~acc vs
  | v :: vs ->
    visited.(v) <- true;
    let center = Graph.around g v |> Iter.head_exn in
    Graph.around g center
    |> Iter.filter (fun u -> u <> v)
    |> Iter.fold   (fun vs u ->
        visited.(u) <- true;
        if deg.(u) = 1 then vs
        else begin
          let w = Graph.around g u |> Iter.filter ((<>) center) |> Iter.head_exn in
          Graph.cut g w u; deg.(w) <- 1;
          w :: vs
        end
      ) vs
    |> solve ~acc:(deg.(center) :: acc)

let () =
  List.range 1 (n+1)
  |> List.filter ~f:(fun v -> deg.(v) = 1)
  |> solve
  |> List.sort ~compare
  |> List.iter ~f:(printf "%d ");
  printf "\n"
