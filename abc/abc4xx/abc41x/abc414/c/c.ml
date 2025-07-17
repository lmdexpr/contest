open Core
open Scanf

module Seq = struct
  include Seq

  let rec (--) start stop () =
    let open Int64 in
    if stop < start then Seq.Nil
    else
      Seq.Cons (start, succ start -- stop)

  let singleton x () =
    Seq.Cons (x, fun () -> Seq.Nil)
end

module Int64 = struct
  include Int64

  let div n d =
    if (n >= zero && d > zero) || (n <= zero && d < zero) || n % d = zero then n / d
    else
      n / d - one

  let rev n =
    let rec rev n acc =
      if n <= 0L then acc
      else
        rev (div n 10L) (acc * 10L + n % 10L)
    in
    rev n 0L
end

let palindromic_numbers d =
  let open Int64 in
  if d <= 1L then Seq.(1L -- 9L)
  else (
    let half = d / 2L in
    let start = pow 10L (pred half) in
    let stop  = pow 10L half - 1L in
    Seq.(start -- stop)
    |> Seq.flat_map (fun x ->
      if d % 2L = 0L then
        Seq.singleton (x * (start * 10L) + rev x)
      else
        Seq.(0L -- 9L)
        |> Seq.map (fun m -> x * (start * 100L) + m * (start * 10L) + rev x)
    )
  )

let palindromic_numbers n =
  let d = Int64.to_string n |> String.length |> Int64.of_int in
  Seq.(1L -- d)
  |> Seq.flat_map palindromic_numbers

let is_palindromic b x =
  let rec s = function
    | 0L -> []
    | x  -> Int64.(x % b) :: s Int64.(x / b)
  in
  let s = List.to_array @@ s x in
  let n = Array.length s in
  let back i = n - 1 - i in
  Iter.(0 -- (n / 2 - 1))
  |> Iter.for_all Int64.(fun i -> s.(i) = s.(back i))

let a = scanf " %Ld" Fn.id
let n = scanf " %Ld" Fn.id

let ans =
  palindromic_numbers n
  |> Seq.take_while Int64.(fun x -> x <= n)
  |> Seq.filter (is_palindromic a)
  |> Seq.fold_left Int64.(+) 0L

let () = printf "%Ld\n%!" ans
