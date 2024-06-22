open Core
open Scanf

let n = scanf "%d" Fn.id

let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let ans = Array.count s ~f:String.(fun s -> s = "Takahashi")

let () = printf "%d\n%!" ans
