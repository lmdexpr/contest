open Core
open Scanf

let t = scanf " %d" Fn.id

let bfs m ~around ~f (init_k, init_v) =
  let tbl = Hashtbl.create m in
  Hashtbl.set tbl ~key:init_k ~data:init_v;
  let rec bfs q =
    match Fqueue.dequeue q with 
    | None        -> tbl
    | Some (v, q) ->
      let data = f @@ Hashtbl.find_exn tbl v in
      around v
      |> Iter.filter (fun   u -> Hashtbl.find tbl u |> Option.is_none)
      |> Iter.fold   (fun q u ->
        Hashtbl.set tbl ~key:u ~data;
        Fqueue.enqueue q u
      ) q
      |> bfs
  in
  bfs @@ Fqueue.singleton init_k

let yes n s =
  let s =
    String.to_array s
    |> Array.map ~f:(function '0' -> true | _ -> false)
  in
  let s = function
    | 0 -> true
    | n -> s.(n - 1)
  in
  let around state =
    Iter.(0 -- (n - 1))
    |> Iter.filter (fun i -> state land (1 lsl i) = 0)
    |> Iter.filter_map (fun i ->
      let state = state lor (1 lsl i) in
      Option.some_if (s state) state
    )
  in
  bfs (module Int) ~f:succ ~around (0, 0)
  |> Fn.flip Hashtbl.find (Int.pow 2 n - 1)
  |> Option.is_some

let () =
  for _ = 1 to t do
    scanf " %d %s" @@ fun n s ->
    let ans = if yes n s then "Yes" else "No" in
    printf "%s\n%!" ans
  done
