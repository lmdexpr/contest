open Core

let rec reverse_in_range a ~start ~stop =
  if start < stop then begin
    Array.swap a start stop;
    reverse_in_range a ~start:(start + 1) ~stop:(stop - 1)
  end

let stop = true, `Stop and continue = false, `Continue

let change_to_next_permutation a ~l ~r =
  Iter.((r - 1) --^ l) |> Iter.fold_while
    (fun _ j ->
       if a.(l) < a.(j) then begin
         Array.swap a l j;
         reverse_in_range a ~start:(l + 1) ~stop:(r - 1);
         stop
       end
       else continue
    )
    false

let next_permutation a ~l ~r = 
  Iter.((r - 2) --^ l) |> Iter.fold_while
    (fun _ i ->
       let success = a.(i) < a.(i + 1) && change_to_next_permutation a ~l:i ~r in
       if success then stop else continue
    )
    false

let permutations n ~f ~acc =
  let nums = Array.init n ~f:ident in
  let rec permutations acc =
    let acc = f acc nums in
    let found_next = next_permutation nums ~l:0 ~r:n in 
    if found_next then permutations acc else acc
  in
  permutations acc

let n = Scanf.scanf "%d" ident

let scan_int _ = Scanf.scanf " %d" ident

let a = Array.init n ~f:(fun _ -> Array.init n ~f:scan_int)

let dislike = Array.make_matrix ~dimx:n ~dimy:n false
let m = scan_int ()
let () =
  for _ = 1 to m do
    let x, y = Scanf.scanf " %d %d" @@ fun x y -> x - 1, y - 1 in
    dislike.(x).(y) <- true;
    dislike.(y).(x) <- true
  done

let inf = 1 lsl 30

let () =
  let f answer p =
    Iter.(0 -- (n - 1))
    |> Iter.fold
      (fun acc i ->
         if i < n - 1 && dislike.(p.(i)).(p.(i + 1)) then None
         else
           Option.map acc ~f:(fun acc -> acc + a.(p.(i)).(i))
      )
      (Some 0)
    |> Option.fold ~init:answer ~f:min
  in
  let answer = permutations n ~acc:inf ~f in
  let answer = if answer = inf then -1 else answer in
  printf "%d\n%!" answer
