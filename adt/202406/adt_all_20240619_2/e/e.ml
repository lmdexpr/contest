open Core
open Scanf

let n = scanf "%d" Fn.id

let repunits = Array.create ~len:12 1L
let () =
  for i = 0 to 10 do 
    repunits.(i + 1) <- Int64.(repunits.(i) * 10L + 1L)
  done
let repunits n = repunits.(n - 1)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let zip3 x y z = zip x @@ zip y z
  let nth_exn n iter = iter |> drop (n - 1) |> head_exn
end

let ans =
  Iter.(zip3 (1 -- 12) (1 -- 12) (1 -- 12))
  |> Iter.map Int64.(fun (a, (b, c)) ->
    repunits a + repunits b + repunits c
  )
  |> Iter.sort ~cmp:Int64.compare
  |> Iter.uniq ~eq:Int64.equal
  |> Iter.nth_exn n

let () = printf "%Ld\n%!" ans
