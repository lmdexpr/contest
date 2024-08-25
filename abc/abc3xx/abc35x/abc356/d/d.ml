open Core
open Scanf

let n, m = scanf "%Ld %Ld" Tuple2.create

let f i =
  let open Int64 in
  let res = (n lsr Int.(i + 1)) lsl i in
  if n land (1L lsl i) = 0L then res
  else
    res + (n land ((1L lsl i) - 1L)) + 1L

let modulo = 998244353L

let ans = 
  Iter.(0 -- 60)
  |> Iter.fold 
    (fun acc i ->
      let open Int64 in
      if m land (1L lsl i) = 0L then acc
      else
        (acc + f i) % modulo
    ) 0L

let () = printf "%Ld\n%!" ans
