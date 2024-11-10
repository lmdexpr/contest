open Core
open Scanf

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  open M

  let rec inverse ?(b = modulo) ?(u = one) ?(v = zero) a =
    if b = zero then (u % modulo + modulo) % modulo
    else
      let t = a / b in
      let a, b = b, a - t * b in
      let u, v = v, u - t * v in
      inverse ~b ~u ~v a

  let ( + ) a b = (a + b) % modulo
  let ( * ) a b = (a * b) % modulo
  let ( / ) a b = a * inverse b
end
module M = Modulo (struct include Int let modulo = 998244353 end)

let (.+()<-) a i x = a.(i) <- M.(a.(i) + x)

let n , m, k = scanf "%d %d %d\n" Tuple3.create

let dp = Array.make_matrix ~dimx:(k + 1) ~dimy:(n + 1) 0
let () =
  dp.(0).(0) <- 1;
  for i = 0 to k - 1 do
    let next = dp.(i + 1) in

    for j = 0 to n - 1 do
      let y = M.(dp.(i).(j) / m) in

      for x = j + 1 to j + m do
        next.+(if x <= n then x else n - (x - n)) <- y
      done
    done;

    next.+(n) <- dp.(i).(n);
  done

let ans = dp.(k).(n)

let () = printf "%d\n%!" ans
