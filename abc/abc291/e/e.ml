open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> [ v ]
    | Some acc -> v :: acc
  let push g v u = update g v ~f:(push u)
 
  let around g v = find g v |> Option.value ~default:[]
end

let g = Graph.create ~size:n (module Int)
let deg = Array.init n ~f:(const 0)
let () =
  for _ = 1 to m do
    let x, y = scanf " %d %d" Tuple2.create in
    let x = x - 1 and y = y - 1 in
    Graph.push g x y;
    deg.(y) <- deg.(y) + 1
  done

let ans = Array.init n ~f:(const 0)
let rec go_through ?(count=0) next =
  ans.(next) <- count + 1;
  let around = Graph.around g next |> List.filter ~f:(fun v ->
      deg.(v) <- deg.(v) - 1;
      deg.(v) = 0
    ) in
  match around with
  | []       -> Some ans
  | [ next ] -> go_through ~count:(count+1) next
  | _        -> None

let result =
  match Array.filter_mapi deg ~f:(fun i d -> Option.some_if (d = 0) i) with
  | [| root |] -> go_through root
  | _          -> None

let () =
  match result with
  | None     -> printf "No\n%!"
  | Some ans ->
    printf "Yes\n";
    Array.iter ans ~f:(printf "%d ");
    printf "\n%!"
