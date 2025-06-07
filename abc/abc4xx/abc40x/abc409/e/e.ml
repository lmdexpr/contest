open Core
open Scanf

let n = scanf " %d" Fn.id

module WeightedGraph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u c = update g v ~f:(push (u, c))
  let connect g v u c = push g v u c; push g u v c

  let around g v = find g v |> Option.value ~default:Iter.empty
end

let x = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let g = WeightedGraph.create ~size:n (module Int)
let () =
  for _ = 1 to n - 1 do
    scanf " %d %d %d" @@ WeightedGraph.connect g
  done

let dfs init =
  let around_fold v p ~init ~f =
    WeightedGraph.around g v
    |> Iter.filter (fun (u, _) -> u <> p)
    |> Iter.fold f init
  in
  let rec dfs ans = function
    | []                 -> ans
    | (v, p, 0) :: stack ->
      dfs ans @@ around_fold v p ~init:((v, p, 1) :: stack) ~f:(fun stack (u, _) -> 
        (u, v, 0) :: stack
      )
    | (v, p, _) :: stack ->
      Fn.flip dfs stack @@ around_fold v p ~init:ans ~f:(fun ans (u, w) -> 
        x.(v-1) <- x.(v-1) + x.(u-1);
        ans + w * abs x.(u-1)
      )
  in
  dfs 0 [ init, -1, 0 ]

let ans = dfs 1

let () = printf "%d\n%!" ans
