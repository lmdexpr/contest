open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

let lines = Array.init m ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)

let same_v_count =
  let cnt = Array.init (n + 1) ~f:(const 0) in
  for i = 0 to m - 1 do
    let l, r = lines.(i) in
    cnt.(l) <- cnt.(l) + 1;
    cnt.(r) <- cnt.(r) + 1
  done;
  Array.fold cnt ~init:0 ~f:(fun ans cnt -> ans + cnt * (cnt - 1) / 2)

let vertical_parallel =
  let size = max n m + 1 in
  let cntl = Array.init size ~f:(const 0)
  and cntr = Array.init size ~f:(const 0) in
  for i = 0 to m - 1 do
    let l, r = lines.(i) in
    cntl.(l - 1) <- cntl.(l - 1) + 1;
    cntr.(r) <- cntr.(r) + 1
  done;
  Iter.(1 -- n)
  |> Iter.fold (fun ans i ->
      cntr.(i) <- cntr.(i) + cntr.(i - 1);
      ans + cntr.(i) * cntl.(i)
    ) 0

let horizontal_parallel =
  let compare (xl, xr) (yl, yr) =
    match compare xr yr with
    | 0 -> compare xl yl
    | i -> i
  in
  Array.sort lines ~compare;
  let cnt = Array.init (n + 1) ~f:(const 0) in
  Array.fold lines ~init:0 ~f:(fun ans (l, r) ->
      let sum = Array.slice cnt (l + 1) r |> Array.sum (module Int) ~f:ident in
      cnt.(l) <- cnt.(l) + 1;
      ans + sum
    )

let () =
  let m = Int64.of_int m in
  let co_events = Int64.of_int @@ same_v_count + vertical_parallel + horizontal_parallel in
  Int64.(m * (m - 1L) / 2L - co_events)
  |> printf "%Ld\n%!"
