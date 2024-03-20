open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let ans, _ =
  Array.fold s ~init:(0, String.Set.empty) ~f:(fun (cnt, set) s ->
    let r = String.rev s in
    if Set.mem set s || Set.mem set r then (cnt, set)
    else
      cnt + 1,
      Set.add set s |> fun set -> Set.add set r
  )

let () = printf "%d\n%!" ans
