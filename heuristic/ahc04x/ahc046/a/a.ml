open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let targets = 
  List.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)
  |> List.rev

module Pos = struct
  type t = int * int
  let (=) (a, b) (c, d) = a = c && b = d
  (*
  let compare = Tuple2.compare ~cmp1:compare ~cmp2:compare

  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
  *)
end

let valid (r, c) = r >= 0 && r < n && c >= 0 && c < n

type direction = Up | Down | Left | Right

let string_of_direction = function
  | Up    -> "U"
  | Down  -> "D"
  | Left  -> "L"
  | Right -> "R" 

let move_next (r, c) = function
  | Up    -> r - 1, c
  | Down  -> r + 1, c
  | Left  -> r, c - 1
  | Right -> r, c + 1

type behavior =
  | Move of direction
  | Slide of direction
  | Action of direction

let string_of_behavior = function
  | Move d   -> "M " ^ string_of_direction d
  | Slide d  -> "S " ^ string_of_direction d
  | Action d -> "A " ^ string_of_direction d

module State = struct
  type t = {
    current : Pos.t;
    next : Pos.t; (* 将来的には複数のターゲットを見る？ *)
    blocks : bool array array;
    turn : int;
    behaviors : behavior Iter.t Iter.t;
  }
  let score_of_state { turn; _ } = turn
  let compare a b =
    Int.ascending (score_of_state a) (score_of_state b)
end

let simulate_slide (i, j) blocks = function
  | Up ->
    let i =
      Iter.(i --^ 0)
      |> Iter.find_pred (fun i -> blocks.(i).(j))
      |> Option.value ~default:0
    in
    i, j
  | Down ->
    let i =
      Iter.(i -- (n - 1))
      |> Iter.find_pred (fun i -> blocks.(i).(j))
      |> Option.value ~default:(n - 1)
    in
    i, j
  | Left ->
    let j =
      Iter.(j --^ 0)
      |> Iter.find_pred (fun j -> blocks.(i).(j))
      |> Option.value ~default:0
    in
    i, j
  | Right ->
    let j =
      Iter.(j -- (n - 1))
      |> Iter.find_pred (fun j -> blocks.(i).(j))
      |> Option.value ~default:(n - 1)
    in
    i, j

let routing State.{ current; next; blocks; _ } =
  let visited = Array.make_matrix ~dimx:n ~dimy:n false in
  let rec bfs q =
    match Fqueue.dequeue_exn q with
    | ((acc, current), _) when Pos.(current = next) -> acc
    | ((acc, current), q) ->
      [ Up; Down; Left; Right ]
      |> List.concat_map ~f:(fun d -> 
        let ni, nj = move_next current d in
        if not @@ valid (ni, nj) || visited.(ni).(nj) then
          []
        else (
          visited.(ni).(nj) <- true;
          [ 
            Iter.singleton (Slide d), simulate_slide current blocks d;
            if blocks.(ni).(nj) then
              Iter.of_list [ Action d; Move d; ], (ni, nj)
            else
              Iter.singleton (Move d), (ni, nj)
          ]
        )
      )
      |> List.fold ~init:q ~f:(fun q (behaviors, next) ->
        let acc = Iter.append acc behaviors in
        Fqueue.enqueue q (acc, next)
      )
      |> bfs
  in
  visited.(fst current).(snd current) <- true;
  bfs Fqueue.(singleton (Iter.empty, current))

let action_blocks state dirs =
  let blocks = Array.copy_matrix state.State.blocks in
  dirs |> Iter.iter (fun dir ->
    let ni, nj = move_next state.current dir in
    if valid (ni, nj) then
      blocks.(ni).(nj) <- not blocks.(ni).(nj)
  );
  State.{ state with 
    blocks; 
    turn = state.turn + Iter.length dirs;
    behaviors = 
      dirs
      |> Iter.map (fun d -> Action d)
      |> Iter.snoc state.behaviors;
  }

let go k state =
  [ []; [Up]; [Down]; [Left]; [Right] ]
  |> List.map ~f:(fun dirs ->
    let dirs = Iter.of_list dirs in
    let behaviors = routing @@ action_blocks state dirs in
    let turn = 
      state.turn + Iter.length behaviors 
    in
    { state with turn; behaviors = Iter.snoc state.behaviors behaviors; }
  )
  |> List.sort ~compare:State.compare
  |> Fn.flip List.take k

let[@warning "-8"] start :: targets = targets

let ans =
  let init = State.{
    current   = start; 
    next      = start; 
    blocks    = Array.make_matrix ~dimx:n ~dimy:n false; 
    turn      = 0; 
    behaviors = Iter.empty;
  } in
  let init = [ init ] in
  let k = 5 in
  let f states target =
    List.concat_map states ~f:(fun state ->
      go k State.{ state with current=state.next; next=target }
    )
    |> List.sort ~compare:State.compare
    |> Fn.flip List.take k
  in
  List.fold ~init ~f targets
  |> List.hd_exn

let () = 
  ans.behaviors
  |> Iter.iter (
    Iter.iter (Fn.compose print_endline string_of_behavior)
  )

