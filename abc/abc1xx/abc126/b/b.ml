open Core
open Scanf

let s = scanf "%d" ident

let pre = s / 100
let suf = s - pre * 100

let is_mm x = 0 < x && x <= 12

let ans =
  match is_mm pre, is_mm suf with
  | true, true   -> "AMBIGUOUS"
  | true, false  -> "MMYY"
  | false, true  -> "YYMM"
  | false, false -> "NA"

let () = printf "%s\n%!" ans
