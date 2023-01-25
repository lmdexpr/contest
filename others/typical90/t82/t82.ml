open Core

let pow10 = Array.init 20 ~f:(const @@ Big_int.big_int_of_int 1)
let () =
  for i = 1 to 19 do
    pow10.(i) <- Big_int.mult_int_big_int 10 pow10.(i - 1)
  done

open Big_int
let m = 1000000007L
let (%) x m = mod_big_int x (big_int_of_int64 m) |> int64_of_big_int
let (/) x m = div_big_int x (big_int_of_int64 m)
let inc = add_int_big_int 1
let dec = add_int_big_int (-1)

let sum n =
  let x, y =
    let n2 = n % 2L in
    if Int64.(n2 = 0L) then
      (n / 2L) % m, inc n % m
    else
      (inc n / 2L) % m, n % m
  in
  Int64.(x * y % m)

let l, r = Scanf.scanf "%Ld %Ld" @@ fun l r -> big_int_of_int64 l, big_int_of_int64 r

let () =
  Iter.(1 -- 19)
  |> Iter.map (fun i ->
      let l = max_big_int l pow10.(Int.(i - 1)) in
      let r = min_big_int r (dec pow10.(i)) in
      let i = Int64.of_int i in
      if gt_big_int l r then i, 0L
      else
        i, Int64.((sum r - sum (dec l) + m) % m)
    )
  |> Iter.fold Int64.(fun acc (i, e) -> (acc + i * e) % m) 0L
  |> printf "%Ld\n%!"
