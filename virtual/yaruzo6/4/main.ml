open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let modulo = 1_000_000_007
let( *%) x y = (x * y) % modulo

let () =
  if abs (n - m) > 1 then begin
    printf "0\n%!"; exit 0
  end

let memorize f =
  let memo = Array.create ~len:100_001 None in
  let rec g x =
    match memo.(x) with
    | Some y -> y
    | None ->
      let y = f g x in
      memo.(x) <- Some y; y
  in
  g

let fact = memorize (fun self ->
    function
    | 0 -> 1
    | x -> x *% self (x - 1)
  )

let () =
  let ans = fact n *% fact m in
  printf "%d\n%!" (if n = m then 2 *% ans else ans)
