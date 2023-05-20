open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" ident))
let b = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" ident))

let rot90 a =
  Array.init n ~f:(fun i ->
      Array.init n ~f:(fun j ->
          a.(n - 1 - j).(i)
        )
    )

let eq = function
  | 1 -> (=) 1
  | _ -> const true

let yes =
  Array.equal (Array.equal eq) a b ||
  let a = rot90 a in
  Array.equal (Array.equal eq) a b ||
  let a = rot90 a in
  Array.equal (Array.equal eq) a b ||
  let a = rot90 a in
  Array.equal (Array.equal eq) a b

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
