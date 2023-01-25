open Core

let rec count_divisible ?(acc=0) n p =
  if n = 1 || n % p <> 0 then n, acc
  else
    count_divisible ~acc:(acc + 1) (n / p) p

let count_factorize n =
  let sqrt = float n |> sqrt |> int_of_float in
  let factors_count, n =
    Iter.(2 -- sqrt)
    |> Iter.fold
      (fun (acc, n) p ->
         let n, count = count_divisible n p in
         let acc = if count = 0 then acc else count + acc in
         (acc, n)
      )
      (0, n)
  in
  factors_count + if n > 1 then 1 else 0

let rec count_magic ?(acc=0) = function
  | 1 -> acc
  | n -> count_magic ~acc:(acc+1) (n / 2 + n % 2)

let n = Scanf.scanf "%d" ident

let () =
  count_factorize n
  |> count_magic
  |> printf "%d\n%!"
