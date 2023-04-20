(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_11_C&lang=ja *)
open Scanf
open Printf

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

let bfs () =
  let dist = Array.init (n+1) @@ fun _ -> -1 in
  let q = Queue.create () in Queue.push (0, 1) q;
  let dequeue q = try Some (Queue.pop q) with Queue.Empty -> None in
  let rec bfs () = 
    match dequeue q with
    | None        -> ()
    | Some (c, v) ->
      if dist.(v) = -1 then dist.(v) <- c;
      Graph.around g v
      |> List.filter (fun u -> dist.(u) = -1)
      |> List.iter   (fun u -> Queue.push (c + 1, u) q);
      bfs ()
  in
  bfs (); dist

let dist = bfs ()
let () =
  for i = 1 to n do
    printf "%d %d\n%!" i dist.(i)
  done
