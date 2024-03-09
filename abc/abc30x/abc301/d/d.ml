open Core
open Scanf

let s = scanf "%s" String.rev
let n = scanf " %Ld" ident

let last = String.length s - 1

open Int64 

let all_zero =
  String.foldi s ~init:0L ~f:(fun i acc -> function
    | '1' -> acc lor (1L lsl i)
    | _   -> acc
  )

let ans =
  if n < all_zero then -1L
  else
    Iter.(last --^ 0)
    |> Iter.filter Char.(fun i -> s.[i] = '?')
    |> Iter.fold (fun acc i ->
        let next = acc lor (1L lsl i) in
        if n < next then acc
        else
          next
      )
      all_zero

let () = printf "%Ld\n" ans
