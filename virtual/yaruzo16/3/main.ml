open Core
open Scanf

let _ = scanf "%s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id
let _ = scanf " %s" Fn.id

let a =
  [|
    "ABGGEGBCFEBFBAF";
    "FFGFACCCECDGCDGAFFFACGDA";
    "EEDCAEAFBDDEEDGGA";
    "GDCAGFFAACBGEDBAFBCDECGAE";
    "EDB";
    "GADGADEDBCGABDDCBBDBEAD";
    "GADBB";
    "DFCE";
    "BFGCGCBEDC";
    "EDGADBGGDDFEEGGFDGCAFBFGFAAD";
    "DDAEBGACDFDGDAB";
    "EEDCECFFAE";
    "ADDBEEABFEAB";
    "FEEBFDGAADAE";
    "GB";
  |]
let () = Array.sort a ~compare:String.compare

let ans = a.(6)

let () = printf "%s\n%!" ans
