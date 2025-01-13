open Core
open Scanf

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.append [| init |] @@ Array.folding_map a ~init ~f:(paired f)

let cumsum2d ~init ~f a =
  let n = Array.length a and m = Array.length a.(0) in
  let a = Array.map a ~f:(cumsum ~init ~f) in
  for i = 1 to n - 1 do
    for j = 0 to m do
      a.(i).(j) <- f a.(i).(j) a.(i-1).(j)
    done
  done;
  Array.append [| Array.create ~len:(m+1) init |] a

let h, w = scanf "%d %d" Tuple2.create
let k, v = scanf " %Ld %Ld" Tuple2.create

let a = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> scanf " %Ld" Fn.id))

let cumsum = cumsum2d ~init:0L ~f:Int64.(+) a
let solve a b c d =
  let open Int64 in
  cumsum.(c).(d) - (cumsum.(a).(d) + cumsum.(c).(b)) + cumsum.(a).(b)

let ans =
  Iter.(product (1 -- h) (1 -- w))
  |> Iter.flat_map (fun (sx, sy) ->
    Iter.(product (sx -- h) (sy -- w))
    |> Iter.map (fun (gx, gy) -> sx-1, sy-1, gx, gy)
  )
  |> Iter.filter_map (fun (sx, sy, gx, gy) ->
    let s = (gx - sx) * (gy - sy) in
    Option.some_if Int64.(k * of_int s + solve sx sy gx gy <= v) s
  )
  |> Iter.max ~lt:(<)
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
