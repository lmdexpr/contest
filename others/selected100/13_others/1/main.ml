open Core
open Scanf

let n = scanf " %d" Fn.id

type color = 
  | White of int
  | Black of int

let solve acc i c =
  let rec compress = function
    | White w :: White w' :: xs -> White (w + w') :: compress xs
    | Black b :: Black b' :: xs -> Black (b + b') :: compress xs
    | lst                       -> lst
  in
  compress @@
  match i % 2, c, acc with
  | 0, Black b, White w :: xs -> Black (b + w) :: xs
  | 0, White w, Black b :: xs -> White (w + b) :: xs
  | _                         -> c :: acc

let ans =
  Iter.(1 -- n)
  |> Iter.fold (fun acc i ->
    solve acc i @@
    scanf " %d" @@ function
    | 0 -> White 1
    | _ -> Black 1
  ) []

let ans = ans
  |> List.fold ~init:0 ~f:(fun acc -> function
    | White w -> acc + w
    | Black _ -> acc
  )

let () = printf "%d\n%!" ans
