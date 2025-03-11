open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create
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
  for _ = 1 to m do
    let u, v, w = scanf " %d %d %Ld" Tuple3.create in
    WeightedGraph.connect g u v w;
  done

let dfs init =
  let ans = ref Int64.max_value in
  let rec dfs = function
    | [] -> !ans

    | (Some ans', _, v) :: rest when v = n ->
      ans := Int64.min !ans ans';
      dfs rest

    | (ans', paths, v) :: rest ->
      WeightedGraph.around g v
      |> Iter.filter (fun (u, _) -> not @@ Set.mem paths u)
      |> Iter.fold (fun rest (u, w) -> 
        let ans' =
          match ans' with
          | None    -> w
          | Some w' -> Int64.(w lxor w')
        in
        (Some ans', Set.add paths u, u) :: rest
      ) rest
      |> dfs
  in
  dfs [ None, Int.Set.singleton init, init ]

let ans = dfs 1

let () = printf "%Ld\n%!" ans
