open Scanf
open Printf

let id x = x

module Modulo (M : sig 
  type t
  val modulo : t 
  val one : t
  val zero : t
  val ( * ) : t -> t -> t
  val ( % ) : t -> t -> t
  val num_bits : int
  val (lsl) : t -> int -> t
  val (land) : t -> t -> t
end) = struct
  open M

  let power a b =
    Array.init num_bits id
    |> Array.fold_left (fun (p, q) i ->
        if b land (one lsl i) <> zero then p * q % modulo, q * q % modulo
        else
          p, q * q % modulo
      ) (one, a)
    |> fst

  let ( ** ) a b = power a b
end

module M = Modulo(struct
  type t = int
  let modulo = 1_000_000_007
  let num_bits = 32
  let one = 1
  let zero = 0
  let ( * ) = ( * )
  let ( % ) = ( mod )
  let (lsl) = ( lsl )
  let (land) = ( land )
end)

let m, n = scanf "%d %d" (fun x y -> x, y)

let ans = M.(m ** n)

let () = printf "%d\n%!" ans
