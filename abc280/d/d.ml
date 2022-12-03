open Core

let k = Scanf.scanf "%Ld" ident

let rec count_divisible ?(acc=0L) n p =
  let open Int64 in
  if n = 1L || n % p <> 0L then n, acc
  else
    count_divisible ~acc:(acc + 1L) (n / p) p

let factorize n =
  let open Int64 in
  let sqrt = Big_int.(big_int_of_int64 n |> sqrt_big_int |> int_of_big_int) in
  let factors, n =
    Iter.(2 -- sqrt)
    |> Iter.fold
      (fun (acc, n) p ->
         let p = of_int p in
         let n, count = count_divisible n p in
         let acc = if count = 0L then acc else (p, count) :: acc in
         (acc, n)
      )
      ([], n)
  in
  if n > 1L then (n, 1L) :: factors else factors

let step2 p a =
  let open Int64 in
  Iter.(1 -- to_int_exn a)
  |> Iter.fold_while
    (fun (n, a) _ ->
       if a <= 0L then (n, a), `Stop
       else
         let n = n + p in
         let _, c = count_divisible n p in
         (n, a - c), `Continue
    )
    (0L, a)
  |> Tuple2.get1

let () =
  factorize k
  |> Iter.of_list
  |> Iter.map (Tuple2.uncurry step2)
  |> Iter.max_exn ~lt:Int64.(<)
  |> printf "%Ld\n%!"
