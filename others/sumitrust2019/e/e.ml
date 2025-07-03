open Core
open Scanf

let modulo = 1000000007
let( *%) a b = (a * b) % modulo

module Array = struct
  include Array
  let ch f a i = a.(i) <- f a.(i)
  let incr = ch succ
  let decr = ch pred
end

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans =
  let cnt = Array.create ~len:(n + 1) 0 in
  cnt.(0) <- 3;
  try
    Array.fold a ~init:1 ~f:(fun acc a ->
      if cnt.(a) <= 0 then invalid_arg "no hat";

      Array.decr cnt a;
      Array.incr cnt (a+1);

      acc *% (cnt.(a) + 1)
    )
  with _ -> 0

let () = printf "%d\n%!" ans
