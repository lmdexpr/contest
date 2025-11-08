open Core
open Scanf

let n = scanf " %d" Fn.id

let a = Array.make_matrix ~dimx:1502 ~dimy:1502 0
let () =
  for _ = 1 to n do
    let x = scanf " %d" Fn.id in
    let y = scanf " %d" Fn.id in
    a.(x).(y) <- a.(x).(y) + 1
  done

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

let cumsum = cumsum2d ~init:0 ~f:Int.(+) a

let solve (a, b) (c, d) =
  let c, d = c + 1, d + 1 in
  cumsum.(c).(d) - (cumsum.(a).(d) + cumsum.(c).(b)) + cumsum.(a).(b)

let q = scanf " %d" Fn.id

let () =
  for _ = 1 to q do
    let a = scanf " %d" Fn.id in
    let b = scanf " %d" Fn.id in
    let c = scanf " %d" Fn.id in
    let d = scanf " %d" Fn.id in

    let ans = solve (a, b) (c, d) in

    printf "%d\n" ans
  done
