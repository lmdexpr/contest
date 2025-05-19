open Core
open Scanf

let modulo = 1000000007
let ( +% ) a b = (a + b) % modulo
and ( -% ) a b = (a - b + modulo) % modulo
let ( *% ) a b = (a * b) % modulo

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans =
  Array.fold a ~init:((1, 0), (0, 0))
    ~f:(fun ((pos_count, neg_count), (pos_sum, neg_sum)) a ->
      let count = pos_count +% neg_count, pos_count in
      let sum =
        pos_sum +% neg_sum +% pos_count *% a,
        pos_sum            -% neg_count *% a
      in
      count, sum
    )
  |> snd |> Tuple2.uncurry (+%)
      
let () = printf "%d\n%!" ans
