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

module Direction = struct
  type h = [ `L | `R ]
  type v = [ `U | `D ]

  type t = [ h | v | `S ]

  let to_char = function
    | `U -> 'U'
    | `D -> 'D'
    | `L -> 'L'
    | `R -> 'R'
    | `S -> 'S'

  let to_delta = function
    | `U -> (-1, 0)
    | `D -> (1, 0)
    | `L -> (0, -1)
    | `R -> (0, 1)
    | `S -> (0, 0)
end

module Point = struct
  type t = { x : int; y : int }

  let create y x = { x; y }
  
  let input () = scanf " %d %d" create
  
  let in_square p l r =
    l <= p.x && p.x < r && l <= p.y && p.y < r

  let (=) p1 p2 = p1.x = p2.x && p1.y = p2.y

  let (+$) p d =
    let dy, dx = Direction.to_delta d in
    { x = p.x + dx; y = p.y + dy }

  let to_tuple p = (p.x, p.y)
  let compare p1 p2 = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (to_tuple p1) (to_tuple p2)
  let sexp_of_t p = Tuple2.sexp_of_t sexp_of_int sexp_of_int (to_tuple p)
  let hash = Hashtbl.hash
end

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let initial_pos = Array.init m ~f:(fun _ -> Point.input ())

let v_walls = Array.init n ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) '1'))
let v_walls Point.{ x; y; } : Direction.h -> bool = function
  | `L -> 0 < x   && v_walls.(y).(x-1)
  | `R -> x < n-1 && v_walls.(y).(x)

let h_walls = Array.init (n-1) ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) '1'))
let h_walls Point.{ x; y; } : Direction.v -> bool = function
  | `U -> 0 < y   && h_walls.(y-1).(x)
  | `D -> y < n-1 && h_walls.(y).(x)

let pass (p: Point.t) = function
  | #Direction.v as d -> not (h_walls p d)
  | #Direction.h as d -> not (v_walls p d)
  | `S                -> true

let attempt_move (p: Point.t) (d: Direction.t) =
  let np = Point.(p +$ d) in
  Option.some_if (Point.in_square p 0 n && Point.in_square np 0 n && pass p d) np

let bfs (start: Point.t) =
  let dist =
    bfs (module Point) (start, 0) ~f:succ ~around:(fun p ->
      Iter.of_list [ `U; `D; `L; `R; ] 
      |> Iter.filter_map (attempt_move p)
    )
  in
  Array.init n ~f:(fun y ->
    Array.init n ~f:(fun x ->
      Hashtbl.find dist (Point.create x y) |> Option.value ~default:(-1)
    )
  )

let () = Random.self_init ()

(* 1. Area Division *)
let dist = Array.map initial_pos ~f:bfs
let my_cells = Array.create ~len:m []
let () =
  for y = 0 to n - 1 do
    for x = 0 to n - 1 do
      let min_dist = ref Int.max_value in 
      let owner = ref (-1) in
      for i = 0 to m - 1 do
        let d = dist.(i).(y).(x) in
        if 0 <= d && d < !min_dist then (
          min_dist := dist.(i).(y).(x);
          owner := i
        );
      done;
      my_cells.(!owner) <- Point.{ x; y; } :: my_cells.(!owner)
    done
  done

(* 2. Key Configuration *)
let config = Array.make_matrix ~dimx:k ~dimy:m `S
let () =
  let all_moves = [| `U; `D; `L; `R|] in
  for i = 0 to 3 do
    for j = 0 to m - 1 do config.(i).(j) <- all_moves.(i) done
  done;
  let all_moves_with_s = [| `U; `D; `L; `R; `S|] in
  for i = 4 to k - 1 do
    for j = 0 to m - 1 do config.(i).(j) <- all_moves_with_s.(Random.int 5) done
  done

(* 3. Simulation & Action Selection *)
let solve ratio =
  let robot_pos = Array.copy initial_pos in
  let painted = Array.make_matrix ~dimx:n ~dimy:n false in
  let unpainted_count = ref (n * n) in
  let actions = ref [] in

  for i = 0 to m - 1 do
    let Point.{ x; y; } = robot_pos.(i) in
    eprintf "Robot %d starts at (%d, %d)\n" i x y;
    if not painted.(y).(x) then (
      painted.(y).(x) <- true;
      decr unpainted_count;
    )
  done;

  let targets = Array.mapi dist ~f:(fun i dist ->
    let compare (p1: Point.t) (p2: Point.t) = compare (dist.(p1.y).(p1.x)) (dist.(p2.y).(p2.x)) in
    let sorted_cells = List.sort ~compare my_cells.(i) in
    ref sorted_cells
  ) in

  let current_targets  = Array.create ~len:m None in
  let target_dist_maps = Array.create ~len:m (lazy (Array.make_matrix ~dimx:n ~dimy:n (-1))) in

  let update_target i =
    let rec reduce = function
      | Point.{ x; y; } :: ps when painted.(y).(x) -> reduce ps
      | ps -> ps
    in
    targets.(i) := reduce !(targets.(i));
    match !(targets.(i)) with
    | [] -> ()
    | new_target :: _ ->
      current_targets.(i) <-
      match current_targets.(i) with
      | Some old_target when Point.(old_target = new_target) -> None
      | _ ->
        target_dist_maps.(i) <- lazy (bfs new_target);
        Some new_target
  in
  for i = 0 to m - 1 do update_target i done;

  let max_turns = 2 * n * n in
  for _ = 1 to max_turns do
    if !unpainted_count > 0 then (
      let best_button = ref (-1) in
      let max_score = ref (-1_000_000_000) in

      for btn = 0 to k - 1 do
        let new_paint_count = ref 0 in
        let dist_reduction  = ref 0 in
        for r_idx = 0 to m - 1 do
          let p = robot_pos.(r_idx) in
          let np = attempt_move p config.(btn).(r_idx) |> Option.value ~default:p in
          if not painted.(np.y).(np.x) then
            incr new_paint_count;
          current_targets.(r_idx) |> Option.iter ~f:(fun _ ->
            let dist_map = Lazy.force target_dist_maps.(r_idx) in
            let current_dist = dist_map.(p.y).(p.x) in
            let next_dist = dist_map.(np.y).(np.x) in
            if current_dist > -1 && next_dist > -1 then
              dist_reduction := !dist_reduction + (current_dist - next_dist)
          )
        done;

        let score = !new_paint_count * ratio + !dist_reduction in
        if score > !max_score then (
          max_score := score;
          best_button := btn
        )
      done;

      let selected_button = if !best_button = -1 then Random.int 4 else !best_button in
      actions := selected_button :: !actions;

      for i = 0 to m - 1 do
        let p = robot_pos.(i) in
        let np = attempt_move p config.(selected_button).(i) |> Option.value ~default:p in
        robot_pos.(i) <- np;
        if not painted.(np.y).(np.x) then (
          painted.(np.y).(np.x) <- true;
          decr unpainted_count
        );
        current_targets.(i)
        |> Option.filter ~f:Point.(fun target -> target = np)
        |> Option.iter   ~f:(fun _ -> update_target i)
      done
    )
  done;
  !unpainted_count, List.rev !actions

let unpainted_count, actions = 
  Iter.(int_range_by 1 1000 ~step:50)
  |> Iter.map solve
  |> Iter.min_exn ~lt:(fun (c1, _) (c2, _) -> c1 < c2)

let () =
  eprintf "Unpainted cells: %d\n" unpainted_count;

  for i = 0 to k - 1 do
    for j = 0 to m - 1 do
      printf "%c " (Direction.to_char config.(i).(j));
    done;
    printf "\n"
  done;

  List.iter ~f:(printf "%d\n") actions;
