(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1167&lang=jp *)
open Scanf
open Printf

let ans     = Array.init 1_000_001 (fun _ -> 1_000_000_000)
let ans_odd = Array.init 1_000_001 (fun i -> i)

let tet_num = Array.init 301 (fun i -> i * (i + 1) * (i + 2) / 6)

let rec loop ?(j=1) i = 
  if tet_num.(j) <= i then begin
    ans.(i) <- min ans.(i) (ans.(i - tet_num.(j)) + 1);
    if tet_num.(j) mod 2 = 1 then
      ans_odd.(i) <- min ans_odd.(i) (ans_odd.(i - tet_num.(j)) + 1);
    loop i ~j:(j+1)
  end

let () =
  ans.(0) <- 0; ans_odd.(0) <- 0;
  for i = 1 to 1_000_000 do
    loop i
  done

let solve = function
  | 0 -> exit 0
  | n -> printf "%d %d\n" ans.(n) ans_odd.(n)
let () =
  scanf "%d" solve;
  while true do
    scanf " %d" solve
  done
