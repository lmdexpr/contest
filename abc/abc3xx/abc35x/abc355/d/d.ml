open Core
open Scanf

let n = scanf "%d" Fn.id

let lr = 
  List.init n ~f:(fun i -> [scanf " %d" Fn.id, i; scanf " %d" Fn.id, i])
  |> List.concat
  |> List.sort ~compare:(Tuple2.compare ~cmp1:compare ~cmp2:compare)

let ans, _, _ =
  let init = 0, -1, Int.Set.empty in
  List.fold lr ~init ~f:(fun (ans, last, set) (v, i) ->
    if Set.mem set i then
      ans, v, Set.remove set i
    else
      ans + Set.length set + Bool.to_int (v = last), last, Set.add set i
  )

let () = printf "%d\n%!" ans
