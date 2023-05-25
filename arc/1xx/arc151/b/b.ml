open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let p = Array.init n ~f:(fun _ -> scanf " %d" @@ fun x -> x - 1)

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  include M

  let rec inverse ?(b = modulo) ?(u = one) ?(v = zero) a =
    if b = zero then (u % modulo + modulo) % modulo
    else
      let t = a / b in
      let a, b = b, a - t * b in
      let u, v = v, u - t * v in
      inverse ~b ~u ~v a
  let power a b =
    Array.init num_bits ~f:ident
    |> Array.fold ~init:(one, a) ~f:(fun (p, q) i ->
        if b land (one lsl i) <> zero then p * q % modulo, q * q % modulo
        else
          p, q * q % modulo
      )
    |> Tuple2.get1

  let ( + ) a b = (a + b) % modulo
  let ( * ) a b = (a * b) % modulo
  let ( / ) a b = a * inverse b

  let ( ** ) a b = power a b
end

module Modulo998244353 = Modulo (struct include Int let modulo = 998244353 end)

let ans, _ =
  let uf = Array.init n ~f:Union_find.create in
  Array.foldi p ~init:(0, n) ~f:(fun i (ans, n) p ->
      if Union_find.same_class uf.(i) uf.(p) then ans, n
      else begin
        Union_find.union uf.(i) uf.(p);
        Modulo998244353.(ans + m ** (n - 1) * (m - 1) / 2), n - 1
      end
    )

let () = printf "%d\n%!" ans
