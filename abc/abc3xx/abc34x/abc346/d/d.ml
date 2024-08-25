open Core
open Scanf

let n = scanf "%d" Fn.id
let s = scanf " %s" String.to_array |> Array.map ~f:Char.((=) '0')
let c = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let (.+()<-) a i x = a.(i) <- Int64.(+) a.(i) x

let f0 = Array.create ~len:(n+1) 0L
let f1 = Array.create ~len:(n+1) 0L
let () =
  for i = 0 to n - 1 do
    f0.(i + 1) <- f0.(i); f1.(i + 1) <- f1.(i);
    let f1, f0 = if i % 2 = 0 then f1, f0 else f0, f1 in
    let f = if s.(i) then f1 else f0 in
    f.+(i + 1) <- c.(i)
  done

let g0 = Array.create ~len:(n+1) 0L
let g1 = Array.create ~len:(n+1) 0L
let () =
  for i = n - 1 downto 0 do
    g0.(i) <- g0.(i + 1); g1.(i) <- g1.(i + 1);
    let g1, g0 = if i % 2 = 0 then g0, g1 else g1, g0 in
    let g = if s.(i) then g1 else g0 in
    g.+(i) <- c.(i)
  done

let ans = 
  Iter.(1 -- (n - 1))
  |> Iter.map Int64.(fun i -> min (f0.(i) + g0.(i)) (f1.(i) + g1.(i)))
  |> Iter.fold Int64.min Int64.max_value

let () = printf "%Ld\n%!" ans
