open Core

let n = Scanf.scanf "%d" ident

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
let a = Array.init (n - 1) ~f:(const 0)
let b = Array.init (n - 1) ~f:(const 0)

let () =
  for i = 0 to n - 2 do
    let ai, bi = Scanf.scanf " %d %d" Tuple2.create in
    Graph.connect g ai bi;
    a.(i) <- ai - 1;
    b.(i) <- bi - 1
  done

let dp = Array.init n ~f:(const 0)
let rec dfs pred now =
  dp.(now - 1) <-
    1 + (
      Graph.around g now
      |> Iter.filter ((<>) pred)
      |> Iter.map (dfs now)
      |> Iter.sum
    );
  dp.(now - 1)

let _ = dfs 0 1

let () =
  Iter.(0 -- (n - 2))
  |> Iter.map (fun i -> min dp.(a.(i)) dp.(b.(i)))
  |> Iter.map (fun r -> r * (n - r))
  |> Iter.sum
  |> printf "%d\n%!" 
