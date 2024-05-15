open Core
open Scanf

let n = scanf "%d" Fn.id
let items =
  List.init n ~f:Int64.(fun _ -> scanf " %Ld %Ld" @@ fun t d -> t, t + d)
  |> List.sort ~compare:(Tuple2.compare ~cmp1:Int64.compare ~cmp2:Int64.compare)

module Heap = struct 
  include Batteries.Heap
  let singleton e = add e empty
  let pop heap    = find_min heap, del_min heap
end

let next heap items t =
  let will_do, items = List.split_while items ~f:(fun (s, _) -> Int64.(s = t)) in
  let heap = Heap.merge heap @@ Heap.of_list (List.map ~f:snd will_do) in
  heap, items

let rec solve ~ans ~t (heap, items) =
  if Heap.size heap = 0 then 
    match items with
    | []              -> ans
    | (t, e) :: items -> solve ~t ~ans @@ next Heap.(singleton e) items t
  else
    let heap, items = next heap items t in
    let e, heap     = Heap.pop heap in
    if Int64.(e < t) then solve (heap, items) ~t ~ans
    else
      solve (heap, items) ~t:(Int64.succ t) ~ans:(ans + 1)

let ans = solve (Heap.empty, items) ~t:0L ~ans:0

let () = printf "%d\n%!" ans
