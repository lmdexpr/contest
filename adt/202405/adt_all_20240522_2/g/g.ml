open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let idxes_of_peek_colors = Array.create ~len:(n+1) []

let a = Array.init m ~f:(fun i ->
  let a = List.init (scanf " %d" Fn.id) ~f:(fun _ -> scanf " %d" Fn.id) |> List.rev in
  let h = List.hd_exn a in
  idxes_of_peek_colors.(h) <- i :: idxes_of_peek_colors.(h);
  a
)

let pop i =
  match a.(i) with
  | [] | [ _ ]   -> a.(i) <- []; None
  | _ :: n :: tl ->
    a.(i) <- n :: tl;
    idxes_of_peek_colors.(n) <- i :: idxes_of_peek_colors.(n);
    Option.some_if (List.length idxes_of_peek_colors.(n) = 2) n

let rec solve queue =
  match Fqueue.dequeue queue with
  | None            -> ()
  | Some (c, queue) ->
    match idxes_of_peek_colors.(c) with
    | i :: j :: idxes ->
      let queue = pop i |> Option.fold ~f:Fqueue.enqueue ~init:queue in
      let queue = pop j |> Option.fold ~f:Fqueue.enqueue ~init:queue in
      idxes_of_peek_colors.(c) <- idxes;
      solve queue
    | _ ->
      assert false
let () = 
  Iter.(1 -- n) |> Iter.filter_map (fun c ->
    Option.some_if (List.length idxes_of_peek_colors.(c) = 2) c
  )
  |> Iter.fold Fqueue.enqueue Fqueue.empty
  |> solve

let ans = if Array.for_all a ~f:List.is_empty then "Yes" else "No"

let () = printf "%s\n%!" ans
