open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

module Graph = struct 
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let get g v = find g v |> Option.value ~default:[]
end

module SI = Set.Make(Int)
let color = Array.init (n + 1) ~f:(const 0)
let coloring g v =
  let s = Stack.singleton (1, v) in
  let rec dfs (b, w) =
    match Stack.pop s with
    | None        ->
      let b = Set.length b and w = Set.length w in
      Some (b * (b - 1) / 2 + w * (w - 1) / 2)
    | Some (c, v) ->
      color.(v) <- c;
      let around = Graph.get g v in 
      match List.find around ~f:(fun u -> color.(u) = c) with
      | Some _ -> None
      | _ ->
        List.iter around ~f:(fun u -> if color.(u) = 0 then Stack.push s (-c, u));
        let b = if c > 0 then Set.add b v else b
        and w = if c < 0 then Set.add w v else w in
        dfs (b, w)
  in
  dfs (SI.empty, SI.empty)

let () =
  let g = Graph.create ~size:(n + 1) (module Int) in
  for _ = 1 to m do
    Scanf.scanf " %d %d" (Graph.connect g)
  done;
  let ans = n * (n - 1) / 2 - m in
  let c =
    Iter.(1 -- n)
    |> Iter.filter_map (fun v -> if color.(v) = 0 then Some (coloring g v) else None)
    |> Iter.fold (fun acc v ->
        match acc, v with
        | None, _ | _, None -> None
        | Some sum, Some c  -> Some (sum + c)
      )
      (Some 0)
  in
  match c with
  | None   -> printf "0\n%!"
  | Some c -> printf "%d\n%!" (ans - c)
