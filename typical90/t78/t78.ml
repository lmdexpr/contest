open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

module Graph = struct 
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)
end

let g  = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    let a, b = Scanf.scanf " %d %d" Tuple2.create in
    let a, b = min a b, max a b in
    Graph.push g b a
  done

let () =
  Graph.count g ~f:(fun adjacents -> List.length adjacents = 1)
  |> printf "%d\n%!"
