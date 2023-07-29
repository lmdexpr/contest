open Core
open Scanf

let modulo = 998244353
let (+%) a b = (a + b) % modulo

let s = scanf "%s" Fn.id
let n = String.length s

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0

let () =
  dp.(0).(0) <- 1;
  for i = 0 to n - 1 do
    match s.[i] with
    | '(' -> 
        for j = 0 to n - 1 do
          dp.(i+1).(j+1) <- dp.(i+1).(j+1) +% dp.(i).(j)
        done
    | ')' -> 
        for j = 1 to n do
          dp.(i+1).(j-1) <- dp.(i+1).(j-1) +% dp.(i).(j)
        done
    | _   -> 
        for j = 0 to n - 1 do
          dp.(i+1).(j+1) <- dp.(i+1).(j+1) +% dp.(i).(j)
        done;
        for j = 1 to n do
          dp.(i+1).(j-1) <- dp.(i+1).(j-1) +% dp.(i).(j)
        done
  done

let ans = dp.(n).(0) % modulo

let () = printf "%d\n%!" ans
