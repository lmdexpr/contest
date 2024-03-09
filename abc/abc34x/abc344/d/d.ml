open Core
open Scanf

let t, n = scanf "%s %d" Tuple2.create

let l = String.length t

let count_match from s m = 
  let rec count_match i = 
    if i >= m then true
    else if Char.(t.[from + i] = s.[i]) then count_match (i + 1)
    else false
  in
  count_match 0

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(l + 1) 1_000_000_000
let () =
  dp.(0).(0) <- 0;
  for i = 1 to n do
    for j = 0 to l do
      dp.(i).(j) <- dp.(i - 1).(j)
    done;
    let a = scanf " %d" Fn.id in
    for _ = 1 to a do
      let s = scanf " %s" Fn.id in
      let m = String.length s in
      for j = 0 to l - m do
        if count_match j s m then
          dp.(i).(j + m) <- 
          Int.min dp.(i).(j + m) (dp.(i - 1).(j) + 1)
      done
    done
  done

let ans = 
  if dp.(n).(l) < 1_000_000_000 then dp.(n).(l)
  else
    -1

let () = printf "%d\n%!" ans
