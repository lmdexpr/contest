open Scanf
open Printf

module Int64 = struct
  include Int64
  let  (+)  = add
  let  (-)  = sub
  let ( * ) = mul
end

let id x = x

let solve () =
  let n, p = scanf " %d %Ld" @@ fun n p -> n, p in
  let a = Array.init n (fun _ -> scanf " %Ld" id) in
  let b = Array.init n (fun _ -> scanf " %Ld" id) in
  let c = Array.init n (fun i -> a.(i), b.(i)) in
  let n = Int64.of_int n in
  Array.sort (fun (la, lb) (ra, rb) -> compare (lb, Int64.neg la) (rb, Int64.neg ra)) c;
  let ans, _ = Array.fold_left (fun (ans, remain) (a, b) ->
    let cost = min p b in
    let residents = min remain a in
    Int64.(ans + cost * residents, remain - residents)
  ) (p, Int64.pred n) c
  in
  printf "%Ld\n" ans

let () =
  for _ = 1 to scanf "%d" id do
    solve ()
  done
