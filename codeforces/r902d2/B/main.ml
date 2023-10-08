open Scanf
open Printf

let id x = x

let solve () =
  let n, p = scanf " %d %d" @@ fun n p -> n, p in
  let a = Array.init n (fun _ -> scanf " %d" id) in
  let b = Array.init n (fun _ -> scanf " %d" id) in
  let c = Array.init n (fun i -> a.(i), b.(i)) in
  Array.sort (fun (la, lb) (ra, rb) -> compare (lb, -la) (rb, -ra)) c;
  let ans, _ = Array.fold_left (fun (ans, remain) (a, b) ->
    let cost = min p b in
    let residents = min remain a in
    ans + cost * residents, remain - residents
  ) (p, n - 1) c
  in
  printf "%d\n" ans

let () =
  for _ = 1 to scanf "%d" id do
    solve ()
  done
