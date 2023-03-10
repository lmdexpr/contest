open Core
open Scanf

let n, h = scanf "%Ld %Ld" Tuple2.create
let a, b, c, d, e = scanf " %Ld %Ld %Ld %Ld %Ld" @@ fun a b c d e -> a, b, c, d, e

let ans =
  Iter.(0 -- Int64.to_int_exn n)
  |> Iter.map Int64.of_int
  |> Iter.filter_map
    (fun x ->
       let open Int64 in
       if h + (b + e) * x - e * n > 0L then Some (a * x)
       else 
         let satiety = h + b * x and fasting = (n - x) * e in
         let y = - (satiety - fasting) / (d + e) + 1L in
         Option.some_if (x + y <= n) @@ a * x + c * y
    )
  |> Iter.min_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
