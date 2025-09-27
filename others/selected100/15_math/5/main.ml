open Core
open Scanf

let m = scanf " %d" Fn.id

let n = Array.init m ~f:(fun _ -> scanf " %Ld %Ld" Tuple2.create)

let ans =
  let sum, ketasuu =
    Array.fold n ~init:(0L, 0L) ~f:Int64.(fun (sum, ketasuu) (d, c) ->
      (sum + d * c, ketasuu + c)
      )
  in
  Int64.(pred sum / 9L + pred ketasuu)

let () = printf "%Ld\n%!" ans
