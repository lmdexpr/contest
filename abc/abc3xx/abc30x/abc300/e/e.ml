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
module M = Modulo (struct include Int64 let modulo = 998244353L end)

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

let n = scanf "%Ld" Fn.id

let dp = Memo.recursive (module Int64) Int64.(fun dp i ->
  if i > n then 0L
  else if i = n then 1L
  else
    Iter.(2 -- 6)
    |> Iter.map (fun j -> dp (i * of_int j))
    |> Iter.fold M.(+) 0L
    |> fun v -> M.(v / 5L)
  )

let () = printf "%Ld\n%!" @@ dp 1L
