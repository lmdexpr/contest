open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let _l = scanf " %d" Fn.id

let s = Array.create ~len:n ""
let p = Array.create ~len:n 0
let () =
  for i = 0 to n - 1 do
    scanf " %s %d" @@ fun si pi ->
    s.(i) <- si;
    p.(i) <- pi;
  done

let ordered_by_p =
  Iter.(0 -- pred n)
  |> Iter.sort ~cmp:(fun i j -> Int.descending p.(i) p.(j))

let all_chars = [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |]
let all_chars_len = Array.length all_chars

module Model = struct
  type t = {
    c : char array;
    a : int array array;
    score : float;
  }

  let initialize_A_row m =
    let a = Array.create ~len:m 0 in
    for _ = 1 to 100 do
      let i = Random.int m in
      a.(i) <- a.(i) + 1
    done;
    a

  let score { c; a; _ } =
    let ord c = Char.(to_int c - to_int 'a') in
    let transition = Array.make_matrix ~dimx:all_chars_len ~dimy:all_chars_len 0. in
    let count = Array.make_matrix ~dimx:all_chars_len ~dimy:all_chars_len 0 in
    for i = 0 to all_chars_len - 1 do
      Iter.(0 -- pred m)
      |> Iter.filter (fun j -> ord c.(j) = i)
      |> Iter.iter (fun j ->
        Array.iteri a.(j) ~f:(fun k a ->
          transition.(i).(ord c.(k)) <- transition.(i).(ord c.(k)) +. float a /. 100.;
          count.(i).(ord c.(k)) <- count.(i).(ord c.(k)) + 1;
        )
      )
    done;
    for i = 0 to all_chars_len - 1 do
      for j = 0 to all_chars_len - 1 do
        if count.(i).(j) > 0 then
          transition.(i).(j) <- transition.(i).(j) /. float count.(i).(j)
      done
    done;
    ordered_by_p
    |> Iter.take 5
    |> Iter.fold (fun score i ->
      let s   = s.(i) in
      let len = String.length s in
      let rec aux acc i =
        if len - 1 <= i then acc
        else
          let acc = acc *. transition.(ord s.[i]).(ord s.[i + 1]) in
          aux acc (i + 1)
      in
      score +. float (p.(i) * p.(i)) *. aux 1. 0 *. float len
    ) 0.

  let create c a =
    let init = { c; a; score = 0.; } in
    let score = score init in
    { init with score }

  let init m =
    create
      Array.(init m ~f:(fun i -> all_chars.(i % all_chars_len)))
      Array.(init m ~f:(fun _ -> initialize_A_row m))

  let (<) { score = s1; _ } { score = s2; _ } = Float.(s1 < s2)

  let max m1 m2 = if m1 < m2 then m2 else m1

  let print { c; a; _ } =
    Array.iteri c ~f:(fun i c ->
      printf "%c " c; Array.iter a.(i) ~f:(printf "%d "); printf "\n"
    )
end

module SimulatedAnnealing = struct
  let get_neighbor_solution Model.{ c; a; _ } =
    let a = Array.copy_matrix a in

    for i = 0 to m - 1 do
      let found = ref false in
      for _ = 1 to 5 do
        if not !found then
          let j1 = Random.int m in
          let j2 = Random.int m in
          if j1 <> j2 then
            if 0 < a.(i).(j1) && a.(i).(j2) < 100 then (
              a.(i).(j1) <- a.(i).(j1) - 1;
              a.(i).(j2) <- a.(i).(j2) + 1;
              found := true
            )
      done;

      if not !found then
        a.(i) <- Model.initialize_A_row m;
    done;

    Model.create c a

  let t_start = 200000.0
  let t_stop  = 0.01
  let cool_rate = 0.999

  let update_model temp best model neighbor =
    if Model.(model < neighbor) then
      Model.max best neighbor,
      let diff = Float.(neighbor.score - model.score) in
      if Float.(1e-9 < temp && Random.float 1.0 < exp (diff / temp)) then
        neighbor
      else
        model
    else
      best, model

  let run () =
    let start_time = Time_float.now () in
    let get_current_span () = Time_float.(diff (now ()) start_time) in

    let limit = Time_float.Span.of_sec 1.8 in

    let rec sa best model temp =
      let current_span = get_current_span () in
      Format.eprintf "%a <= %a\n%!" Time_float.Span.pp limit Time_float.Span.pp current_span;
      Model.print best;
      Format.eprintf "%f\n%!" best.score;
      if Time_float.Span.(limit <= current_span) || Float.(temp <= t_stop) then best
      else (
        let neighbor = get_neighbor_solution model in

        let best, model = update_model temp best model neighbor in

        sa best model @@ temp *. cool_rate
      )
    in
    let init = Model.init m in
    sa init init t_start
end

let () = 
  SimulatedAnnealing.run ()
  |> Model.print
