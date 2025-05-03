open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

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

let yes =
  let init = 1 in
  let rec search c visited v =
    let visited = Set.add visited v in
    let f = Set.mem visited in
    let c = c + 1 in
    match Graph.around g v |> Iter.to_list with
    | [ u; w ] when f u && not (f w) -> search c visited w
    | [ u; w ] when f w && not (f u) -> search c visited u
    | [ u; w ] when (u = init || w = init) && c = m -> true
    | _ -> false
  in
  m = n &&
  search 1 Int.Set.(singleton init) Graph.(around g init |> Iter.head_exn)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
