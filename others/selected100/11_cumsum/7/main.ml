open Scanf
open Printf

let len = 24 * 60 * 60

let to_sec h m s = h * 60 * 60 + m * 60 + s

let solve n =
  let imos = Array.make (len+2) 0 in
  for _ = 1 to n do
    scanf " %d:%d:%d %d:%d:%d" @@ fun lh lm ls rh rm rs ->
    let l = to_sec lh lm ls and r = to_sec rh rm rs in
    imos.(l) <- imos.(l) + 1;
    imos.(r) <- imos.(r) - 1;
  done;
  for i = 1 to len do
    imos.(i) <- imos.(i) + imos.(i-1)
  done;
  let ans = Array.fold_left max 0 imos in
  printf "%d\n%!" ans

let () =
  let n = ref 0 in
  scanf "%d" (fun x -> n := x);
  while !n > 0 do
    solve !n;
    scanf " %d" (fun x -> n := x);
  done
