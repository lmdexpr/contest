open Core

let n = Scanf.scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %Ld" ident)
let () = Array.sort a ~compare:Int64.compare

let modulo = 998244353L

let b = Array.init n ~f:(const 0L)
let _recurrence_relation =
  b.(n - 1) <- a.(n - 1);
  for i = n - 2 downto 0 do
    let i_1 = i + 1 in
    b.(i) <- Int64.( (2L * (b.(i_1) - a.(i_1)) % modulo + a.(i_1) + a.(i)) % modulo )
  done

let ans =
  Iter.(n - 1 --^ 0)
  |> Iter.fold
    Int64.(fun acc i -> (acc + a.(i) * b.(i) % modulo) % modulo)
    0L

let () = printf "%Ld\n%!" ans
