(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_1_A&lang=ja *)
open Scanf
open Printf

module Graph = struct
  include Hashtbl
  let push g v u w = add g v (u, w)

  let around g v = find_all g v
end

let v, e, r = scanf "%d %d %d" @@ fun v e r -> v, e, r

let g = Graph.create v
let () =
  for _ = 1 to e do
    scanf " %d %d %d" @@ Graph.push g
  done

let rec dijkstra dist q =
  try
    let v = Queue.take q in
    let f (u , cost) =
      if dist.(u) > dist.(v) + cost then begin
        dist.(u) <- dist.(v) + cost;
        Queue.push u q
      end
    in
    List.iter f @@ Graph.around g v;
    dijkstra dist q
  with _ -> ()

let inf = 1000_000_000

let dist = Array.init v @@ fun _ -> inf
let q  = Queue.create ()
let () =
  dist.(r) <- 0;
  Queue.push r q;
  dijkstra dist q

let () =
  for i = 0 to v - 1 do
    if dist.(i) = inf then
      printf "INF\n"
    else
      printf "%d\n%!" @@ dist.(i)
  done
