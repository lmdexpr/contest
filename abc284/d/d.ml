open Core

let sqrt n = Big_int.(big_int_of_int64 n |> sqrt_big_int |> int64_of_big_int)

let test n =
  let open Int64 in
  let rec find i = 
    if n % i = 0L then i
    else
      find (i + 1L)
  in
  let a = find 2L in
  if n / a % a = 0L then a, n / a / a
  else
    sqrt (n / a), a

let t = Scanf.scanf "%d" ident
let () =
  for _ = 1 to t do
    let n = Scanf.scanf " %Ld" ident in
    let p, q = test n in
    printf "%Ld %Ld\n" p q
  done
