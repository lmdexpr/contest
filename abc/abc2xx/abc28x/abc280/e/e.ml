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

  let ( + ) a b = (a % modulo + b % modulo) % modulo
  let ( * ) a b = (a % modulo * b % modulo) % modulo
  let ( / ) a b = a * inverse b
end
module M = Modulo (struct include Int let modulo = 998244353 end)

module Memo = struct
  include Memo
  let recursive m f =
    let h = Hashtbl.create m in
    let rec g x = Hashtbl.update_and_return h x ~f:(function
      | Some v -> v
      | None   -> f g x
    )
    in g
end

let n, p = scanf "%d %d" Tuple2.create

let dp = Memo.recursive (module Int) (fun dp -> function
  | 0 -> 0
  | 1 -> 1
  | x -> M.((1 + dp (x - 2)) * p / 100 + (1 + dp (x - 1)) * (100 - p) / 100)
)

let () = printf "%d\n%!" @@ dp n
