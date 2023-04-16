(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_11_B&lang=jp *)
open Scanf

let n = scanf "%d" @@ fun x -> x

module Graph = struct
  include Hashtbl
  let find_opt g v = try Some (find g v) with Not_found -> None
  let update g v f = replace g v (f @@ find_opt g v)

  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v (push u)

  let around g v =
    match find_opt g v with
    | None    -> []
    | Some vs -> vs
end
let g = Graph.create n
let () =
  for _ = 1 to n do
    let u, k = scanf " %d %d" @@ fun x y -> x, y in
    for _ = 1 to k do
      scanf " %d" @@ Graph.push g u
    done
  done

let dfs () =
  let d = Array.init (n+1) @@ fun _ -> 0 in
  let f = Array.init (n+1) @@ fun _ -> 0 in
  let rec dfs ?(t=1) v = 
    d.(v) <- t;
    Graph.around g v |> List.rev
    |> List.fold_left (fun t u -> if d.(u) <> 0 then t else dfs ~t:(t+1) u) t
    |> fun t ->
    f.(v) <- t + 1; t + 1
  in
  let t = ref 1 in
  for i = 1 to n do
    if d.(i) = 0 then
      t := dfs ~t:!t i + 1
  done;
  d, f

let d, f = dfs ()
let () =
  for i = 1 to n do
    Printf.printf "%d %d %d\n%!" i d.(i) f.(i)
  done
