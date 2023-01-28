open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v
end
let g = Graph.create ~size:n (module Int)

let uf = Array.init (n+1) ~f:Union_find.create

let has_loop =
  Iter.(1 -- m)
  |> Iter.fold
    (fun has_loop _ ->
       if has_loop then true
       else
         let v, u = Scanf.scanf " %d %d" Tuple2.create in
         let has_loop = Union_find.same_class uf.(v) uf.(u) in
         Graph.connect g v u;
         Union_find.union uf.(v) uf.(u);
         has_loop
    )
    false

let degree = Array.init 3 ~f:(const 0)
let () =
  Graph.iter g ~f:(fun v ->
      match List.length v with
      | 1 -> degree.(1) <- degree.(1) + 1
      | 2 -> degree.(2) <- degree.(2) + 1
      | _ -> degree.(0) <- degree.(0) + 1
    )

let yes =
  not has_loop &&
  degree.(1) = 2 &&
  degree.(0) = 0

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
