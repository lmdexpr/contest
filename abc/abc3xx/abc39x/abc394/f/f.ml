open Core
open Scanf

module Memo = struct
  include Memo
  let recursive m f =
    let h = Hashtbl.create m in
    let rec g x = Hashtbl.update_and_return h x ~f:(function
      | Some v -> v
      | None   -> f g x
    )
    in g
end

let n = scanf " %d" Fn.id

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

let degLt v n = Graph.around g v |> Iter.take n |> Iter.length < n

let dfs = Memo.recursive (module Int) @@ fun self p v ->
  if degLt v 4 then 1
  else
    Graph.around g v
    |> Iter.filter ((<>) p)
    |> Iter.map (self v)
    |> Iter.sort ~cmp:Int.descending
    |> Iter.take 3
    |> Iter.fold (fun acc v -> acc + v) 1

let ans =
  Iter.(1 -- n)
  |> Iter.find_pred (fun v -> not @@ degLt v 4)
  |> Option.map  ~f:(fun r -> 1 + dfs 0 r)
  |> Option.value ~default:(-1)

let () = printf "%d\n%!" ans
