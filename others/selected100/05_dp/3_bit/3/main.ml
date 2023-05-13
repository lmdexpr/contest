(* https://atcoder.jp/contests/joi2014yo/tasks/joi2014yo_d *)
open Core
open Scanf

let modulo = 10007
let ( +% ) a b = (a + b) % modulo

let n = scanf "%d" ident
let s = scanf " %s" ident

let bits_of_char = function
  | 'J' -> 0b001
  | 'O' -> 0b010
  | 'I' -> 0b100
  | _   -> assert false

let s =
  String.to_array s
  |> Array.map ~f:bits_of_char
  |> Array.append [| 0b001 |]

let memorize2 ~dimx ~dimy f = 
  let memo = Array.make_matrix ~dimx ~dimy None in
  let rec g x y =
    match memo.(x).(y) with
    | Some v -> v
    | None   -> let v = f g x y in memo.(x).(y) <- Some v; v
  in
  g

module Bits = struct
  let has       lhs rhs = lhs land rhs = rhs
  let has_inter lhs rhs = lhs land rhs <> 0
end

let dp = memorize2 ~dimx:(n + 1) ~dimy:(0b111 + 1) @@fun self day joiner ->
  let pic = s.(day) in
  if day = 1 then 
    Bits.has joiner (pic lor bits_of_char 'J')
    |> Bool.to_int
  else if not @@ Bits.has_inter joiner pic then 0
  else
    Iter.(0b001 -- 0b111)
    |> Iter.filter Bits.(has_inter joiner)
    |> Iter.map    (self @@ day - 1)
    |> Iter.fold   (+%) 0

let ans =
  Iter.(0b001 -- 0b111)
  |> Iter.map  (dp n)
  |> Iter.fold (+%) 0

let () = printf "%d\n%!" ans
