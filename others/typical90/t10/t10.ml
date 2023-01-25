open Core

let n = Scanf.scanf "%d" ident

let score =
  Iter.(1 -- n)
  |> Iter.scan (fun (p1, p2) _ ->
      let c, p = Scanf.scanf " %d %d" Tuple2.create in
      if c = 1 then
        (p + p1, p2)
      else
        (p1, p + p2)
    ) (0, 0)
  |> Iter.to_array

let q = Scanf.scanf " %d" ident
let () =
  for _ = 1 to q do
    let l, r = Scanf.scanf " %d %d" Tuple2.create in
    let r1, r2 = score.(r)
    and l1, l2 = score.(l - 1) in
    printf "%d %d\n%!" (r1 - l1) (r2 - l2)
  done
