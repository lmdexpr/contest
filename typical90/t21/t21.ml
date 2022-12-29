open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v 
    | Some acc -> Iter.snoc acc v
  let push g v u = update g v ~f:(push u)

  let next g v = find g v |> Option.value ~default:Iter.empty
end

let normal  = Graph.create ~size:n (module Int)
and reverse = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    let a, b = Scanf.scanf " %d %d" Tuple2.create in
    Graph.push normal a b;
    Graph.push reverse b a
  done

let[@inline] const_2nd _ b = b
let dfs ~graph ~visit ?(before=const_2nd) ?(after=const_2nd) =
  let rec dfs acc v =
    visit.(v) <- true;
    let acc =
      Graph.next graph v
      |> Iter.filter (fun u -> not visit.(u))
      |> Iter.fold dfs (before v acc)
    in
    after v acc
  in
  dfs Iter.empty

(* 強連結成分分解 (Strongly Connected Component decompose) *)

let visit = Array.init (n + 1) ~f:(const false)
let step1 = dfs ~graph:normal ~visit ~after:Iter.cons
let returns = 
  Iter.(1 -- n) |> Iter.fold
    (fun returns v -> if visit.(v) then returns else Iter.append (step1 v) returns)
    Iter.empty

let visit = Array.init (n + 1) ~f:(const false)
let step2 = dfs ~graph:reverse ~visit ~before:Iter.cons
let ans =
  returns
  |> Iter.map (fun v -> if visit.(v) then 0 else Iter.length (step2 v))
  |> Iter.map (fun c -> c * (c - 1) / 2)
  |> Iter.sum

let () = printf "%d\n%!" ans
