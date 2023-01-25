open Core

let n = Scanf.scanf "%d" ident

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
      let b = SI.length b and w = SI.length w in
      let c = if b > w then 1 else -1 in
      Some c
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
  for _ = 1 to n - 1 do
    Scanf.scanf " %d %d" (Graph.connect g)
  done;
  let c = Option.value_exn (coloring g 1) in
  Iter.of_array_i color
  |> Iter.filter_map (fun (i, c') -> Option.some_if (c = c') i)
  |> Iter.take (n / 2)
  |> Iter.iter (printf "%d ");
  printf "\n%!"
