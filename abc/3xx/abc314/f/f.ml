open Core
open Scanf

let n = scanf "%d" Fn.id

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
let modulo = 998244353
module Modulo998244353 = Modulo (struct
  include Int
  let modulo = modulo
end)

let probs = Array.init n ~f:(const 0)
let teams = Array.init n ~f:Int.Set.singleton

let () =
  for _ = 1 to n - 1 do
    let p, q = scanf " %d %d" Tuple2.create in
    let p, q = p - 1, q - 1 in
    let a, b = Set.length teams.(p), Set.length teams.(q) in
    probs.(p) <- Modulo998244353.(probs.(p) + (a / (a + b)));
    probs.(q) <- Modulo998244353.(probs.(q) + (b / (a + b)));
    let team = Set.union teams.(p) teams.(q) in
    teams.(p) <- team;
    teams.(q) <- team;
  done

let () =
  Array.iter probs ~f:(printf "%d ");
  printf "\n"
