open Core

let n, s = Scanf.scanf "%d %d" Tuple2.create

let lucky_bags = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)

let dp = Array.init (n + 1) ~f:(fun _ -> Array.init (s + 1) ~f:(const false))

let () =
  dp.(0).(0) <- true;
  for i = 1 to n do
    for j = 0 to s do
       let a, b = lucky_bags.(i - 1) in
       if j >= a && dp.(i - 1).(j - a) then dp.(i).(j) <- true;
       if j >= b && dp.(i - 1).(j - b) then dp.(i).(j) <- true;
    done
  done

let rec restoration ?(acc=[]) day wallet =
  if day <= 0 then acc
  else
    let a, b = lucky_bags.(day - 1) in
    let choice = wallet >= a && dp.(day - 1).(wallet - a) in
    let choice ~a ~b = if choice then a else b in
    restoration ~acc:(choice ~a:'A' ~b:'B' :: acc) (day - 1) (wallet - choice ~a ~b)

let () =
  if not dp.(n).(s) then printf "Impossible"
  else
    restoration n s |> List.iter ~f:(printf "%c");
  printf "\n%!"
