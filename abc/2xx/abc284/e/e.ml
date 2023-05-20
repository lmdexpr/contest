open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.snoc acc v
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let next g v = find g v |> Option.value ~default:Iter.empty
end

let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    Scanf.scanf " %d %d" @@ Graph.connect g
  done

let visit = Array.init (n+1) ~f:(const false)
let dfs g init =
  let answer = ref 0 in
  let limit = 1000000 in
  let rec dfs v =
    if !answer = limit then !answer
    else begin
      answer := !answer + 1;
      visit.(v) <- true;
      let answer = 
        Graph.next g v
        |> Iter.filter (fun u -> not visit.(u))
        |> Iter.map dfs
        |> Iter.max ~lt:(<)
        |> Option.value ~default:!answer
      in
      visit.(v) <- false;
      answer
    end
  in
  dfs init

let () = printf "%d\n%!" (dfs g 1)
