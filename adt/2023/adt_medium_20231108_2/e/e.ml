open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_list)
  let start x = by_bits x (bits x)
end

let rec solve ?(acc=0L) i = function
  | []      ->
    let local =
      Iter.(i -- (n - 1))
      |> Iter.map (Array.get a)
      |> Iter.fold Int64.(lor) 0L
    in
    Int64.(acc lxor local)
  | x :: xs ->
    let local =
      Iter.(i -- x)
      |> Iter.map (Array.get a)
      |> Iter.fold Int64.(lor) 0L
    in
    let acc = Int64.(acc lxor local) in
    solve ~acc (x + 1) xs

let ans = 
  Bit_all.start (n - 1)
  |> Iter.map (solve 0)
  |> Iter.min_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
