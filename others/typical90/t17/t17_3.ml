open Core

module BIT = struct
  type 'a t = {
    size: int;
    tree: 'a array
  }

  let create ~size ~init = {
    size;
    tree = Array.init (size + 1) ~f:(const init)
  }

  let rec add bit i x =
    if i > bit.size then ()
    else begin
      bit.tree.(i) <- bit.tree.(i) + x;
      add bit (i + (i land -i)) x
    end

  let rec query ?(acc = 0) bit i =
    if i <= 0 then acc
    else
      query ~acc:(acc + bit.tree.(i)) bit (i - (i land -i))
  let query bit i = query bit i
end

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
  let bit = BIT.create ~size:(n + 2) ~init:0 in
  Array.fold lines ~init:0 ~f:(fun acc (l, r) ->
      let sum = BIT.query bit r - BIT.query bit l in
      BIT.add bit l 1;
      acc + sum
    )

let () =
  let m = Int64.of_int m in
  let co_events = Int64.of_int @@ same_v_count + vertical_parallel + horizontal_parallel in
  printf "%Ld\n%!" Int64.(m * (m - 1L) / 2L - co_events)
