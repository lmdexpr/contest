(* https://atcoder.jp/contests/abc138/tasks/abc138_d *)
open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

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
  for _ = 1 to n - 1 do
    scanf " %d %d" @@ Graph.connect g
  done

let counter = Array.init (n + 1) ~f:(const 0)
let () =
  for _ = 1 to q do
    let p, x = scanf " %d %d" Tuple2.create in
    counter.(p) <- counter.(p) + x
  done

let dfs g =
  let visited = Array.init (n + 1) ~f:(const false) in
  let rec dfs = function
    | []                       -> ()
    | v :: vs when visited.(v) -> dfs vs
    | v :: vs ->
      visited.(v) <- true;
      Graph.around g v
      |> Iter.filter (fun u -> not visited.(u))
      |> Fn.flip Iter.fold vs (fun vs u ->
          counter.(u) <- counter.(u) + counter.(v);
          u :: vs
        )
      |> dfs
  in
  dfs [ 1 ]

let () =
  dfs g;
  for i = 1 to n do
    if i > 1 then printf " ";
    printf "%d" counter.(i)
  done;
  printf "\n%!"
