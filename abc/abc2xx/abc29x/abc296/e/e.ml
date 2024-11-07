open Core
open Scanf

module DirectedGraph = struct
  type t = {
    size : int;
    normal  : (int, int Iter.t) Hashtbl.t;
    reverse : (int, int Iter.t) Hashtbl.t;
  }
  let create ~size = {
    size;
    normal  = Hashtbl.create ~size (module Int);
    reverse = Hashtbl.create ~size (module Int);
  }

  let push v = function
    | None     -> Iter.singleton v 
    | Some acc -> Iter.snoc acc v
  let push g v u = Hashtbl.update g v ~f:(push u)
  let push g v u = push g.normal v u; push g.reverse u v

  let next g v = Hashtbl.find g.normal  v |> Option.value ~default:Iter.empty
  let pred g v = Hashtbl.find g.reverse v |> Option.value ~default:Iter.empty

  let strongly_connected_components g =
    let iterate visit f = 
      Iter.fold 
        (fun acc v -> if visit.(v) then acc else Iter.cons (f v) acc)
        Iter.empty
    in
    let step1 = 
      let visit = Array.init (g.size + 1) ~f:(const false) in
      let rec dfs acc v =
        visit.(v) <- true;
        next g v
        |> Iter.filter (fun u -> not visit.(u))
        |> Iter.fold dfs acc
        |> Iter.cons v
      in
      iterate visit @@ dfs Iter.empty
    in
    let step2 = 
      let visit = Array.init (g.size + 1) ~f:(const false) in
      let rec dfs acc v =
        visit.(v) <- true;
        pred g v
        |> Iter.filter (fun u -> not visit.(u))
        |> Iter.fold dfs (Iter.cons v acc)
      in
      iterate visit @@ dfs Iter.empty
    in
    Iter.(1 -- g.size) |> step1 |> Iter.flatten |> step2
end

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let g = DirectedGraph.create ~size:n
let () =
  Iter.(1 -- n) |> Iter.iter (fun i -> DirectedGraph.push g i a.(i - 1))

let self_cycle = 
  Array.foldi a ~init:0 ~f:(fun i acc a -> acc + Bool.to_int (i + 1 = a))

let ans = 
  DirectedGraph.strongly_connected_components g
  |> Iter.map Iter.length
  |> Iter.filter (fun len -> len > 1)
  |> Iter.sum

let ans = self_cycle + ans

let () = printf "%d\n" ans
