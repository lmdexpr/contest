open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:String.(fun _ -> scanf " %s" @@ fun s -> s = "For")

let yes = Array.count s ~f:Fn.id > n / 2

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
