open Core
open Scanf

let n, t = scanf "%d %d" Tuple2.create

let c = Array.init n ~f:(fun _ -> scanf " %d" ident)
let r = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ts, ones =
  Array.foldi c
    ~init:(Int.Set.empty, Int.Set.empty)
    ~f:(fun i (ts, ones) ci ->
        if ci = t then Set.add ts i, ones
        else if ci = c.(0) then ts, Set.add ones i
        else ts, ones
      )
let set = if Set.is_empty ts then ones else ts

let init = Set.min_elt_exn set
let ans  =
  Set.fold set ~init ~f:(fun ans i ->
      if r.(ans) < r.(i) then i else ans
    )
let ans = ans + 1

let () = printf "%d\n%!" ans
