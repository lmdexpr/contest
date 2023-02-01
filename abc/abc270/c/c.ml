open Core
open Scanf

let n, x, y = scanf "%d %d %d" Tuple3.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let next g v = find g v |> Option.value ~default:Iter.empty
end

let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to n - 1 do
    scanf " %d %d" @@ Graph.connect g
  done

let visit = Array.init (n+1) ~f:(const false)
let ans =
  visit.(x) <- true;
  let s = Stack.singleton (Iter.empty, x) in
  let rec bfs () =
    let acc, v = Stack.pop_exn s in
    visit.(v) <- true;
    if v = y then Iter.snoc acc y
    else begin
      Graph.next g v
      |> Iter.filter (fun u -> not visit.(u))
      |> Iter.iter (fun u -> Stack.push s (Iter.snoc acc v, u));
      bfs ()
    end
  in
  bfs ()

let () = Iter.iter (printf "%d ") ans
