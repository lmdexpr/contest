open Core
open Scanf

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v
 
  let around g v = find g v |> Option.value ~default:Iter.empty
end

let n, m, h = scanf " %d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

(*
type edge = {u : int; v : int; }

let es = Array.init m ~f:(fun _ -> scanf " %d %d" @@ fun u v -> 
  { u; v; }
)

let generate_tree compare =
  let g   = Graph.create ~size:n (module Int) in
  let dsu = Array.init (n+1) ~f:Union_find.create in
  Array.sort es ~compare;
  Array.iter es ~f:(fun {u; v; } ->
    if not @@ Union_find.same_class dsu.(u) dsu.(v) then (
      Graph.connect g u v;
      Union_find.union dsu.(u) dsu.(v);
    )
  );
  g
let value c d { v; u; } = c * a.(v) + d * a.(u)
let g c d = generate_tree (fun e e' -> compare (value c d e) (value c d e'))
*)

let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.connect g
  done

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let solve k =
  let visited = Array.create ~len:n false in
  let ans = Array.create ~len:n (-1) in
  let rec dfs v used path = 
    if Set.length path = h then path
    else
      Graph.around g v
      |> Iter.filter (fun u -> not @@ Set.mem used u)
      |> Iter.filter (fun u -> not visited.(u))
      |> Iter.sort ~cmp:(fun u v -> Int.ascending a.(u) a.(v))
      |> Iter.take k
      |> Iter.map    (fun u -> dfs u Set.(add used u) Set.(add path (v, u)))
      |> Iter.max ~lt:(fun p q -> (Set.length p) < (Set.length q))
      |> Option.value ~default:path
  in
  Iter.(0 -- (n - 1))
  |> Iter.sort ~cmp:(fun i j -> Int.ascending a.(i) a.(j))
  |> Iter.iter (fun v ->
    if not visited.(v) then
      dfs v Int.Set.(singleton v) SP.empty
      |> Set.iter ~f:(fun (v, u) ->
        visited.(v) <- true;
        visited.(u) <- true;
        ans.(u) <- v;
      )
  );
  ans

let solve2 k ans =
  let find_root v = 
    let rec loop h v = 
      if ans.(v) = -1 then h, v
      else 
        loop (h + 1) ans.(v)
    in
    v, loop 0 v
  in
  let roots = Array.init n ~f:find_root in
  let is_orphan (v, (h, _)) = 
    h <= k &&
    Array.for_all roots ~f:(fun (_, (_, p)) -> p <> v)
  in
  let orphans, others = Array.partition_tf roots ~f:is_orphan in
  Array.iter orphans ~f:(fun (v, (l, _)) ->
    let around = Graph.around g v |> Iter.to_array |> Int.Set.of_array in
    let _, p =
      Array.fold others ~init:(0, -1) ~f:(fun (k, p) (u, (k', _)) ->
        if
          Set.mem around u && k < k' && k' < h - l
        then 
          k', u
        else
          k, p
      )
    in
    ans.(v) <- p;
  );
  ans

let () =
  let ans = solve 4 in
  let ans = solve2 0 ans in
  Array.iter ans ~f:(printf "%d "); printf "\n"
