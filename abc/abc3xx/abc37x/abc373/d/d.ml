open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
module Graph = struct
  include Hashtbl
  let push v w = function
    | None     -> Iter.singleton (v, w)
    | Some acc -> Iter.cons (v, w) acc
  let push g v u w = update g v ~f:(push u w)
 
  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let unrefed = Array.create ~len:n true
let () =
  for _ = 1 to m do
    scanf " %d %d %Ld" @@ fun u v w ->
    if Int64.(w < 0L) then
      ( Graph.push g v u (Int64.abs w); unrefed.(u) <- false )
    else
      ( Graph.push g u v w; unrefed.(v) <- false )
  done

let x = Array.create ~len:n (-1L)

let dfs start =
  let rec loop v w =
    x.(v) <- w;
    Graph.around g v
    |> Iter.filter (fun (u, _) -> Int64.(x.(u) = -1L))
    |> Iter.iter (fun (u, w) -> loop u Int64.(w + x.(v)))
  in
  loop start 0L

let () =
  for i = 0 to n - 1 do
    if unrefed.(i) then
      dfs i
  done

let () = 
  Array.iter x ~f:(printf "%Ld "); printf "\n"
