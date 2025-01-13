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

let m, n = scanf "%d %d" Tuple2.create
let k     = scanf " %d" Fn.id

let j = Array.make_matrix ~dimx:m ~dimy:n 0
let o = Array.make_matrix ~dimx:m ~dimy:n 0

let () =
  for x = 0 to m - 1 do
    scanf " %s" @@ String.iteri ~f:(fun y ->function
      | 'J' -> j.(x).(y) <- 1
      | 'O' -> o.(x).(y) <- 1
      | 'I' -> ()
      | _   -> assert false
    );
  done

let j = cumsum2d ~init:0 ~f:(+) j
let o = cumsum2d ~init:0 ~f:(+) o

let solve cumsum a b c d =
  let a, b = a - 1, b - 1 in
  cumsum.(c).(d) - (cumsum.(a).(d) + cumsum.(c).(b)) + cumsum.(a).(b)

let () =
  for _ = 1 to k do
    scanf " %d %d %d %d" @@ fun a b c d ->
    let j = solve j a b c d in
    let o = solve o a b c d in
    let i = (c - a + 1) * (d - b + 1) - j - o in
    printf "%d %d %d\n" j o i
  done
