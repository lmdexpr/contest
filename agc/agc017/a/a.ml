open Core

let n, p = Scanf.scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let ans =
  match Array.findi a ~f:(fun _ a -> a % 2 = 1) with
  | None   -> if p = 0 then Int.pow 2 n else 0
  | Some _ -> Int.pow 2 (n - 1)

let () = printf "%d\n%!" ans
