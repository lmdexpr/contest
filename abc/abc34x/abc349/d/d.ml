open Core
open Scanf

let l, r = scanf "%Ld %Ld" Tuple2.create

let pow2 = Array.create ~len:64 1L
let () =
  for i = 1 to 63 do
    pow2.(i) <- Int64.(pow2.(Int.pred i) * 2L);
  done

let find_i_j n =
  let open Int64 in
  let rec go acc n =
    if n = 0L || n % 2L <> 0L then acc, n
    else go (Int.succ acc) (n / 2L)
  in
  go 0 n

let rec solve ?(front=[]) ?(back=[]) l r =
  let open Int64 in
  if r <= l then List.rev front, back
  else begin
    let li, lj = find_i_j l in
    let ri, rj = find_i_j r in
    if l <> 0L && Int.(li <= ri) then
      let next  = pow2.(li) * (lj + 1L) in
      let front = (l, next) :: front in
      solve ~front ~back next r
    else
      let next = pow2.(ri) * (rj - 1L) in
      let back = (next, r) :: back in
      solve ~front ~back l next
    end
      
let front, back = solve l r

let () =
  printf "%d\n" @@ List.length front + List.length back;
  List.iter ~f:(Tuple2.uncurry @@ printf "%Ld %Ld\n") front;
  List.iter ~f:(Tuple2.uncurry @@ printf "%Ld %Ld\n") back;
