open Core
open Scanf

let n = scanf " %d" Fn.id

let w = Array.make_matrix ~dimx:n ~dimy:n 0
let d = Array.make_matrix ~dimx:n ~dimy:n 0

let () = 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      scanf " %d" @@ Array.set w.(i) j
    done
  done

let () = 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      scanf " %d" @@ Array.set d.(i) j
    done
  done

module Manhattan = struct
  type t = int * int

  let home = (0, 0)

  let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

  let from_home (x, y) = distance home (x, y)
end

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right

  let to_string = function
    | Up    -> "U"
    | Down  -> "D"
    | Left  -> "L"
    | Right -> "R"
end

module Ops = struct
  type t =
    | Pick
    | Pop
    | Move of Direction.t
    [@@warning "-37"]

  let print = function
    | Pick     -> printf "1\n%!"
    | Pop      -> printf "2\n%!"
    | Move dir -> printf "%s\n%!" (Direction.to_string dir)

  let move dir = print @@ Move dir
end

module State = struct
  type t = {
    boxes : (int * int) list;
    exists : bool array array;
    d : int array array;
    pos : Manhattan.t;
  }

  let initial () =
    let exists = Array.make_matrix ~dimx:n ~dimy:n true in
    exists.(0).(0) <- false;
    {
      boxes = [];
      pos = Manhattan.home;
      d = Array.copy_matrix d;
      exists;
    }

  let reset state =
    eprintf "Resetting state\n%!";
    { state with 
      boxes = [];
    }

  let go dir state =
    let (i, j) = state.pos in
    { state with pos = match dir with
      | Direction.Up    -> i - 1, j
      | Direction.Down  -> i + 1, j
      | Direction.Left  -> i, j - 1
      | Direction.Right -> i, j + 1
    }

  let validate state =
    let (i, j) = state.pos in
    0 <= i && i < n && 0 <= j && j < n

  let move_to (gi, gj) state =
    let (i, j) = state.pos in
    let di = gi - i in
    let dj = gj - j in
    if di > 0 then
      for _ = 1 to di do Ops.print (Ops.Move Direction.Down) done;
    if di < 0 then
      for _ = 1 to -di do Ops.print (Ops.Move Direction.Up) done;
    if dj > 0 then
      for _ = 1 to dj do Ops.print (Ops.Move Direction.Right) done;
    if dj < 0 then
      for _ = 1 to -dj do Ops.print (Ops.Move Direction.Left) done;
    { state with pos = (gi, gj) }

  let pick state =
    let i, j = state.pos in
    if not state.exists.(i).(j) then None
    else (
      let exists = Array.copy_matrix state.exists in
      exists.(i).(j) <- false;
      Some { state with boxes = (i, j) :: state.boxes; exists; }
    )
  
  let check { pos; boxes; _ } =
    let l = Manhattan.from_home pos in
    List.folding_map boxes ~init:0 ~f:(fun acc (i, j) ->
      acc + w.(i).(j),
      0 < d.(i).(j) - acc * (l+1)
    )
    |> List.for_all ~f:Fn.id

  let step dir { boxes; d; _ } =
    List.fold boxes ~init:0 ~f:(fun acc (i, j) ->
      d.(i).(j) <- d.(i).(j) - acc;
      acc + w.(i).(j)
    ) |> ignore;
    Ops.move dir

  let rec return_to_home state =
    let i, j = state.pos in
    if i < 0 || j < 0 then
      failwith "Invalid position: out of bounds";
    eprintf "Returning to home from (%d, %d)\n%!" i j;
    if i = 0 && j = 0 then state
    else (
      match
      [ Direction.Up; Direction.Left; ]
      |> List.filter_map ~f:(fun dir ->
        let state = go dir state in
        let (i, j) = state.pos in
        if not (validate state && (i <> 0 || j <> 0)) then None
        else (
          pick state
          |> Option.filter ~f:check
          |> Option.map ~f:(fun state -> dir, state)
        )
      )
      with
      | (dir, state) :: _ ->
        eprintf "Picking box at (%d, %d)\n%!" (fst state.pos) (snd state.pos);
        step dir state;
        Ops.print Ops.Pick;
        return_to_home state
      | _ ->
        let dir =
          if validate (go Direction.Up state) then Direction.Up else Direction.Left
        in
        step dir state;
        state
        |> go dir
        |> return_to_home
    )
end

let sorted =
  Iter.(0 -- pred n)
  |> Iter.flat_map (fun i ->
    Iter.(0 -- pred n) |> Iter.map (fun j -> i, j)
  )
  |> Iter.filter (fun (i, j) -> not (i = 0 && j = 0))
  |> Iter.sort ~cmp:(fun (i1, j1) (i2, j2) ->
    let d1 = Manhattan.from_home (i1, j1) in
    let d2 = Manhattan.from_home (i2, j2) in
    if d1 <> d2 then Int.descending d1 d2
    else
      Int.descending w.(i1).(j1) w.(i2).(j2)
  )

let () =
  sorted
  |> Iter.fold (fun state (i, j) ->
    if not state.State.exists.(i).(j) then state
    else (
      eprintf "Processing box at (%d, %d)\n%!" i j;
      state
      |> State.move_to (i, j)
      |> State.pick
      |> Option.value_map ~default:state ~f:(fun state -> Ops.print Ops.Pick; state)
      |> State.return_to_home
      |> State.reset
    )
  ) State.(initial ())
  |> ignore
