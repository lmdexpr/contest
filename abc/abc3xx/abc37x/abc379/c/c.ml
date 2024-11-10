open Core
open Scanf

let n, m = scanf "%Ld %d" Tuple2.create

let x = Array.init m ~f:(fun _ -> scanf " %Ld" Fn.id)
let a = Array.init m ~f:(fun _ -> scanf " %Ld" Fn.id)

let xa = Array.zip_exn x a
let () = 
  Array.sort xa ~compare:(Tuple2.compare ~cmp1:Int64.compare ~cmp2:Int64.compare);
  if Int64.(fst xa.(0) <> 1L) then (
    printf "-1\n%!";
    exit 0
  )
let xa =
  if Int64.(fst (Array.last xa) = n) then xa
  else
    Array.append xa [|(n, 0L)|]

let ans, rems = 
  Iter.(0 -- (Array.length xa - 2))
  |> Fn.flip Iter.fold_while (0L, 0L) Int64.(fun (ans, rems) i ->
    let x, a = xa.(i) in
    let nx, _ = xa.(Int.succ i) in

    let a = a + rems in

    let steps = nx - x in
    let rems  = a - steps in

    if rems < 0L then (-1L, rems), `Stop
    else
      (ans + steps * (steps - 1L) / 2L + rems * steps, rems), `Continue
  ) 
let ans = if Int64.(snd Array.(last xa) + rems <> 1L) then -1L else ans

let () = printf "%Ld\n%!" ans
