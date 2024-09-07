open Core
open Scanf

let s = scanf  "%s" String.to_array
let t = scanf " %s" String.to_array

let n = Array.length s

let ans =
  Iter.(0 -- (n - 1))
  |> Fn.flip Iter.fold Iter.empty (fun acc i ->
    if Char.(s.(i) > t.(i)) then (
      s.(i) <- t.(i);
      Iter.snoc acc @@ String.of_array s
    ) else
      acc
  )
let ans =
  Iter.((n - 1) --^ 0)
  |> Fn.flip Iter.fold ans (fun acc i ->
    if Char.(s.(i) < t.(i)) then (
      s.(i) <- t.(i);
      Iter.snoc acc @@ String.of_array s
    ) else
      acc
  )

let () = 
  printf "%d\n%!" @@ Iter.length ans;
  Iter.iter (printf "%s\n") ans
