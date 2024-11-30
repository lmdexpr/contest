open Core
open Scanf

let s, q = scanf "%s %d" Tuple2.create

let n = Int64.of_int @@ String.length s

let flip c = Char.(if is_lowercase c then uppercase else lowercase) c

let () =
  for _ = 1 to q do
    let k = scanf " %Ld" Int64.pred in
    
    s.[Int64.(k % n |> to_int_exn)]
    |> (if Int64.(popcount @@ k / n) % 2 = 0 then Fn.id else flip)
    |> printf "%c "
  done;
  printf "\n%!"
