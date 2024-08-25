open Core
open Scanf

let s, t = scanf "%s %s" Tuple2.create
let n = String.length s and m = String.length t

let check c d = match c, d with
  | '?', _ | _, '?' -> 1
  | _               -> Bool.to_int Char.(c = d)

let pre = Array.init n ~f:(const 0)
let suf = Array.init n ~f:(const 0)
let first =
  Iter.(0 -- (m - 1))
  |> Iter.fold (fun acc i ->
      pre.(i)         <- check s.[i]         t.[i];
      suf.(n - m + i) <- check s.[n - m + i] t.[i];
      acc + suf.(n - m + i)
    )
    0

let yes result = printf (if result = m then "Yes\n%!" else "No\n%!"); result

let first = yes first

let next acc x = acc + pre.(x) - suf.(n - m + x) |> yes

let _ = Iter.(0 -- (m - 1)) |> Iter.fold next first
