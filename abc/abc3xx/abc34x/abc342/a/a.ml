open Core
open Scanf

let s = scanf "%s" Fn.id

let chars = Array.init 26 ~f:(const [])
let () =
  String.iteri s ~f:(fun i c ->
    let c = Char.to_int c - Char.to_int 'a' in
    chars.(c) <- i :: chars.(c)
  )

let ans =
  Array.find_exn chars ~f:(fun l -> List.length l = 1)
  |> List.hd_exn
  |> (+) 1

let () = printf "%d\n%!" ans
