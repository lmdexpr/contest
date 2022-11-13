open Core

let scan_3 () = Scanf.scanf " %d %d %d" @@ fun a b c -> a, b, c

let n, m, k = scan_3 ()

module Graph = struct 
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)

  let get g v = find g v |> Option.value ~default:[]
end

let g = Graph.create ~size:400_001 (module Int)

let () =
  for _ = 1 to m do
    let u, v, a = scan_3 () in
    let padding = if a = 1 then 0 else n in
    let u, v = padding + u, padding + v in
    Graph.push g u (v, 1);
    Graph.push g v (u, 1)
  done;
  for _ = 1 to k do
    let s = Scanf.scanf " %d" ident in
    let t = n + s in
    Graph.push g s (t, 0);
    Graph.push g t (s, 0)
  done

let inf = 1000_000_000
let dist = Array.init 400_001 ~f:(const inf)
let () = dist.(1) <- 0

let rec bfs01 q = 
  match Fdeque.dequeue_front q with
  | None -> ()
  | Some (v, q) ->
    let arounds = Graph.get g v in
    let f q (u, cost) =
      if dist.(u) <= dist.(v) + cost then q
      else begin
        dist.(u) <- dist.(v) + cost;
        let dir = if cost = 0 then `front else `back in
        Fdeque.enqueue q dir u
      end
    in
    bfs01 @@ List.fold arounds ~init:q ~f

let () =
  bfs01 @@ Fdeque.singleton 1;
  let ans = min dist.(n) dist.(2 * n) in
  let ans = if ans = inf then -1 else ans in
  printf "%d\n" ans
