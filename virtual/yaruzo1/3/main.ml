open Core
open Scanf

let n = scanf "%d" ident

let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let expected = String.to_array "indeednow"

let () =
  Array.sort expected ~compare:Char.compare;
  Array.iter s ~f:(fun s ->
      Array.sort s ~compare:Char.compare;
      let yes = if Array.equal Char.(=) s expected then "YES" else "NO" in
      printf "%s\n%!" yes
  )
