open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

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
  Array.iteri a ~f:(fun i a ->
    Graph.push normal (i + 1) a;
    Graph.push reverse a (i + 1)
  )

let strongly_connected_components =
  let[@inline] const_2nd _ b = b in
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
  in
  let iterate visit f = 
    Iter.fold 
      (fun acc v -> if visit.(v) then acc else Iter.cons (f v) acc)
      Iter.empty
  in
  let visit = Array.init (n + 1) ~f:(const false) in
  let step1 = dfs ~graph:normal ~visit ~after:Iter.cons in
  let returns = 
    Iter.(1 -- n) |> iterate visit step1 |> Iter.flatten
  in
  let visit = Array.init (n + 1) ~f:(const false) in
  let step2 = dfs ~graph:reverse ~visit ~before:Iter.cons in
  returns |> iterate visit step2

let ans = Array.create ~len:(n + 1) 0
let () = 
  strongly_connected_components |> Iter.iter (fun scc ->
    let len = Iter.length scc in
    if len > 1 then
      Iter.iter (fun u -> ans.(u) <- len) scc
  ) 

let rec path_to_loop ?(acc = 1) v =
  if ans.(v) <> 0 then acc + ans.(v) 
  else 
    path_to_loop ~acc:(acc + 1) a.(v - 1)

let () =
  for i = 1 to n do
    if ans.(i) = 0 && a.(i - 1) = i then
      ans.(i) <- 1
  done;
  for i = 1 to n do
    if ans.(i) = 0 then
      ans.(i) <- path_to_loop a.(i - 1)
  done

let () = printf "%d\n" @@ Array.fold ans ~init:0 ~f:(+)
