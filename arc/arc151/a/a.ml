open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" String.to_array
let t = scanf " %s" String.to_array

let d = Array.counti s ~f:Char.(fun i c -> c <> t.(i))

let u = Array.init n ~f:(const '0')

let () =
  Iter.((n - 1) --^ 0)
  |> Iter.filter Char.(fun i -> s.(i) <> t.(i))
  |> Iter.fold
    (fun diff i ->
       if diff = 0 then diff
       else if diff > 0 && Char.(s.(i) = '1') then (u.(i) <- '1'; diff - 2)
       else if diff < 0 && Char.(t.(i) = '1') then (u.(i) <- '1'; diff + 2)
       else
         diff
    )
    (Array.count s ~f:Char.((=) '1') - Array.count t ~f:Char.((=) '1'))
  |> ignore

let () =
  if d % 2 = 0 then Array.iter u ~f:(printf "%c") else printf "-1";
  printf "\n%!"
