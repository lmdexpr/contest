open Core

let bfs01 s h w rs cs =
  let inf = 1000_000_000 in
  let dist = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> [|inf; inf; inf; inf|])) in
  dist.(rs).(cs) <- [|0; 0; 0; 0|];
  let rec bfs01 q =
    match Fdeque.dequeue_front q with
    | None -> ()
    | Some ((d, r, c), q) ->
      let arounds =
        [0, (r + 1, c); 1, (r, c + 1); 2, (r - 1, c); 3, (r, c - 1)]
        |> List.filter ~f:(fun (_, (r, c)) -> 0 <= r && r < h && 0 <= c && c < w)
      in
      let f q (dn, (rn, cn)) =
        let cost = dist.(r).(c).(d) + Bool.to_int (d <> dn) in
        if Char.(s.(rn).[cn] = '#') || dist.(rn).(cn).(dn) <= cost then q
        else begin
          dist.(rn).(cn).(dn) <- cost;
          let dir = if d = dn then `front else `back in
          Fdeque.enqueue q dir (dn, rn, cn)
        end
      in
      bfs01 @@ List.fold arounds ~init:q ~f
  in
  bfs01 @@ Fdeque.of_list [(0, rs, cs); (1, rs, cs); (2, rs, cs); (3, rs, cs)]
