open Core
open Scanf

let n = scanf "%d" Fn.id

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u w = update g v ~f:(push (u, w))
  let connect g v u w = push g v u w; push g u v w

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g   = Graph.create ~size:n (module Int)
let sum = ref 0L
let () =
  for _ = 1 to n - 1 do
    let a, b, c = scanf " %d %d %Ld" Tuple3.create in
    sum := Int64.(!sum + c);
    Graph.connect g a b c
  done

let sum = !sum

let dfs start =
  let dist = Array.create ~len:(n+1) (-1L) in
  let rec dfs = function
    | []        -> dist
    | u :: rest ->
      let open Int64 in
      Graph.around g u
      |> Iter.filter (fun (v, _) -> dist.(v) = -1L)
      |> Iter.fold 
        (fun rest (v, w) -> dist.(v) <- dist.(u) + w; v :: rest) 
        rest
      |> dfs
  in
  dist.(start) <- 0L;
  dfs [ start ]

let farthest start =
  dfs start
  |> Array.mapi ~f:Tuple2.create
  |> Array.max_elt ~compare:(fun (_, a) (_, b) -> Int64.compare a b)
  |> Option.value_exn

let diameter = farthest 1 |> fst |> farthest |> snd

let ans = Int64.(2L * sum - diameter)

let () = printf "%Ld\n" ans
