open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

let sum =
  Iter.(0 -- (n - 1))
  |> Iter.scan Int64.(fun acc i -> acc + a.(i)) 0L
  |> Iter.to_array

let init =
  Iter.(0 -- (m - 1))
  |> Iter.map Int64.(fun i -> (of_int i + 1L) * a.(i))
  |> Iter.fold Int64.(+) 0L

let ans =
  Iter.(1 -- (n - m))
  |> Fn.flip Iter.scan init (fun p l ->
      let r = l + m - 1 in
      let l = l - 1 in
      Int64.( p - (sum.(r) - sum.(l)) + of_int m * a.(r) )
    )
  |> Iter.max_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
