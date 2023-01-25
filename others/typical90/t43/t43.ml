open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let scan_pair () = Scanf.scanf " %d %d" @@ fun r c -> r - 1, c - 1
let rs, cs = scan_pair ()
let rt, ct = scan_pair ()

let s = Array.init h ~f:(fun _ -> Scanf.scanf " %s" ident)

let inf = 1000_000_000
let dist = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> [|inf; inf; inf; inf|]))
let () = dist.(rs).(cs) <- [|0; 0; 0; 0|]
let init_q = Fdeque.of_list [(0, rs, cs); (1, rs, cs); (2, rs, cs); (3, rs, cs)]

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

let () =
  bfs01 init_q;
  Array.min_elt ~compare dist.(rt).(ct)
  |> Option.value ~default:0
  |> printf "%d\n%!"
