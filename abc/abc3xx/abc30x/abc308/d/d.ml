open Core
open Scanf

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

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let dist =
  bfs (module struct
    type t = int * int
    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
    let hash = Hashtbl.hash
  end)
    ((0, 0), 0) ~f:succ ~around:(fun (x, y) ->
    Iter.of_list [
      (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)
    ]
    |> Iter.filter (fun (x', y') ->
      0 <= x' && x' < w && 0 <= y' && y' < h &&
      match s.(y).(x), s.(y').(x') with
      | 's', 'n'
      | 'n', 'u'
      | 'u', 'k'
      | 'k', 'e'
      | 'e', 's' -> true
      | _        -> false
    )
  )

let yes = Option.is_some @@ Hashtbl.find dist (w-1, h-1)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
