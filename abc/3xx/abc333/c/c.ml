open Core
open Scanf

let n = scanf "%d" Fn.id

let rec repunits ?(acc=1L) = function
  | 1 -> acc
  | n -> repunits ~acc:Int64.(acc * 10L + 1L) (n - 1)
let repunits = Array.init 12 ~f:(fun i -> repunits (i + 1))
let repunits n = repunits.(n - 1)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let zip3 x y z = zip x @@ zip y z
end

let ans =
  Iter.(zip3 (1 -- 12) (1 -- 12) (1 -- 12))
  |> Iter.map Int64.(fun (a, (b, c)) ->
    repunits a + repunits b + repunits c
  )
  |> Iter.sort ~cmp:Int64.compare
  |> Iter.uniq ~eq:Int64.equal
  |> Iter.drop (n - 1)
  |> Iter.head_exn

let () = printf "%Ld\n%!" ans
