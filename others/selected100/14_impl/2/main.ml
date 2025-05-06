open Core
open Scanf

let run_length_compress a =
  let n = Array.length a in
  let rec loop i now (len, acc) =
    if n <= i then Iter.snoc acc (now, len)
    else
      loop (i + 1) a.(i) @@
      if now = a.(i) then len + 1, acc
      else
        1, Iter.snoc acc (now, len)
  in
  if n = 0 then Iter.empty
  else
    loop 1 a.(0) (1, Iter.empty)

let find_max_repeats a =
  run_length_compress a
  |> Iter.filter (fun(x, _) -> x > 0)
  |> Iter.fold
    (fun (now, (s, m)) (_, l) -> 
      now + l, if m < l then (now, l) else (s, m)
    )
    (0, (0, 0))
  |> snd

let solve h w k a =
  let step p =
    for i = h - 2 downto 0 do
      for j = 0 to w - 1 do
        let k =
          Iter.(succ i -- pred h)
          |> Iter.find_pred (fun k -> a.(k).(j) > 0)
          |> Option.value ~default:h
        in
        if a.(k-1).(j) = 0 then (
          a.(k-1).(j) <- a.(i).(j);
          a.(i).(j)   <- 0
        );
      done;
    done;
    Iter.(0 -- pred h)
    |> Iter.map (fun i -> i, find_max_repeats a.(i))
    |> Iter.filter (fun (_, (_, len)) -> k <= len)
    |> Iter.map (fun (i, (start, len)) ->
      let score = Int64.(of_int a.(i).(start) * of_int len * (2L ** p)) in
      for j = start to start + len - 1 do
        a.(i).(j) <- 0
      done;
      score
    )
    |> Iter.fold Int64.(+) 0L
  in
  Seq.ints 0
  |> Seq.map (fun i -> step @@ Int64.of_int i)
  |> Seq.take_while Int64.(fun score -> score > 0L)
  |> Seq.fold_left Int64.(+) 0L

let h, w, k = scanf " %d %d %d" Tuple3.create

let c = Array.init h ~f:(fun _ ->
  scanf " %s" String.to_array
  |> Array.map ~f:(fun d -> Char.to_int d - Char.to_int '0')
)

let ans =
  Iter.(product
    (1 -- pred h)
    (0 -- pred w)
  )
  |> Iter.map (fun (i, j) ->
    let a = Array.copy_matrix c in
    a.(i).(j) <- 0;
    solve h w k a
  )
  |> Iter.fold Int64.max 0L

let () =
  printf "%Ld\n%!" ans

