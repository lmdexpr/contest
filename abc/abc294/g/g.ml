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
 
  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let e = Array.init (n+1) ~f:(fun _ -> Array.init (n+1) ~f:(const 0))
let () =
  for _ = 1 to n - 1 do
    let v, u, w = scanf " %d %d %d" Tuple3.create in
    Graph.connect g v u;
    e.(v).(u) <- w;
    e.(u).(v) <- w
  done

let vin  = Array.init (n+1) ~f:ident
let vout = Array.init (n+1) ~f:ident
let route =
  let visit = Array.init (n+1) ~f:(const false) in
  let rec dfs v route =
    if visit.(v) then route
    else begin
      visit.(v) <- true;
      Graph.around g v
      |> Iter.filter (fun u -> not visit.(u))
      |> Iter.fold (fun route u -> dfs u route) route
    end
  in
  dfs 1 Iter.empty |> Iter.to_array

let ans = 0

let () = printf "%d\n%!" ans
