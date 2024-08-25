open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let s = Array.init n ~f:(fun _ -> scanf " %s" ident)
let t = Array.init m ~f:(fun _ -> scanf " %s" ident)

let ans =
  Array.count s ~f:String.(
      fun s -> Array.exists t ~f:(fun t -> t = drop_prefix s 3)
    )

let () = printf "%d\n%!" ans
