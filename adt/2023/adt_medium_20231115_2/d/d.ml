open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
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
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.connect g
  done

let () =
  for i = 1 to n do
    let around = Graph.around g i in
    printf "%d " @@ Iter.length around;
    Iter.sort ~cmp:compare around |> Iter.iter (printf "%d ");
    printf "\n"
  done
