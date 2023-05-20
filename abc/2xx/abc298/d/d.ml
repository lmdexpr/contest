open Core
open Scanf

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  open M

  let power a b =
    Array.init num_bits ~f:ident
    |> Array.fold ~init:(one, a) ~f:(fun (p, q) i ->
      if b land (one lsl i) <> zero then p * q % modulo, q * q % modulo
      else
        p, q * q % modulo
      )
    |> Tuple2.get1

  let ( + ) a b = (a + b) % modulo
  let ( - ) a b = (a - b) % modulo
  let ( * ) a b = (a * b) % modulo
  let ( ** ) a b = power a b
end
module Modulo998244353 = Modulo (struct include Int let modulo = 998244353 end)

let q  = scanf "%d" ident
let () =
  Iter.(1 -- q)
  |> Iter.fold (fun (q, acc) _ ->
      match scanf " %d" ident with
      | 1 ->
        let acc = acc * 10 in
        let x   = scanf " %d" ident in
        Fqueue.enqueue q x, Modulo998244353.(acc + x)
      | 2 ->
        let h, q = Fqueue.dequeue_exn q in
        let p    = Fqueue.length q in
        q, Modulo998244353.(acc - (h * 10 ** p))
      | 3 -> printf "%d\n" acc; q, acc
      | _ -> q, acc
    )
    (Fqueue.singleton 1, 1)
  |> ignore
