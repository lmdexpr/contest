open Core
open Scanf

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
let e = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) false
let () =
  for _ = 1 to n - 1 do
    let u, v = scanf " %d %d" Tuple2.create in
    Graph.connect g u v;
    e.(u).(v) <- true; e.(v).(u) <- true;
  done

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let bfs init =
  let visited = Array.init (n+1) ~f:(const (-1)) in
  visited.(init) <- 0;
  let q = Queue.singleton init in
  let rec bfs () =
    match Queue.dequeue q with
    | None   -> ()
    | Some v ->
      Graph.around g v
      |> Iter.filter (fun u -> visited.(u) = -1)
      |> Iter.iter (fun u -> 
        visited.(u) <- visited.(v) + 1;
        Queue.enqueue q u
      );
      bfs ()
  in
  bfs (); visited

let ans =
  Iter.(1 -- n)
  |> Iter.map (fun i -> i, bfs i)
  |> Iter.fold (fun acc (i, visited) ->
    Iter.((i + 1) -- n)
    |> Iter.filter (fun j -> 
      visited.(j) > 2 && visited.(j) % 2 = 1 &&
      not e.(i).(j)
    )
    |> Iter.fold (fun acc j -> Set.add acc (i, j)) acc
  ) SP.empty

let is_first = Set.length ans % 2 = 1

let () =
  printf "%s\n%!" @@ if is_first then "First" else "Second";

  let rec loop ans =
    match Set.min_elt ans with
    | None        -> assert false
    | Some (i, j) ->
      printf "%d %d\n%!" i j;
      let ans = Set.remove ans (i, j) in
      let i, j = scanf " %d %d" Tuple2.create in
      let i, j = min i j, max i j in
      loop @@ Set.remove ans (i, j)
  in

  if is_first then loop ans
  else
    let i, j = scanf " %d %d" Tuple2.create in
    loop @@ Set.remove ans (i, j)
