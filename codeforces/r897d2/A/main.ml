open Scanf
open Printf

let id x = x

let solve n a =
  let a   = Array.mapi (fun i a -> a, i) a in Array.sort compare a;
  let ans = Array.init n id in
  for i = n downto 1 do
    let _, j = a.(n - i) in
    ans.(j) <- i
  done;
  Array.iter (printf "%d ") ans; printf "\n"

let t = scanf "%d" id
let () =
  for _ = 1 to t do
    let n = scanf " %d" id in
    let a = Array.init n (fun _ -> scanf " %d" id) in
    solve n a
  done
