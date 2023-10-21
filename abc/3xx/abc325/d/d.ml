open Core
open Scanf

let n = scanf "%d" Fn.id

let items = Array.init n ~f:Int64.(fun _ -> scanf " %Ld %Ld" @@ fun t d -> t, t + d)
let () =
  Array.sort items ~compare:(Tuple2.compare ~cmp1:Int64.compare ~cmp2:Int64.compare)

module Heap = struct
  include Batteries.Heap
  let singleton x = add x empty
end
let rec solve ?(t=0L) ?(ans=0) (heap, items) =
  let go heap items t =
    let will_do, items = List.split_while items ~f:(fun (s, _) -> Int64.(s = t)) in
    let heap = Heap.merge heap @@ Heap.of_list (List.map ~f:snd will_do) in
    heap, items
  in
  match Heap.size heap, items with
  | 0, []              -> ans
  | 0, (t, e) :: items -> solve ~t ~ans @@ go (Heap.singleton e) items t
  | _, items ->
    let heap, items = go heap items t in
    let e    = Heap.find_min heap in
    let heap = Heap.del_min heap in
    if Int64.(e < t) then solve (heap, items) ~t ~ans
    else
      solve (heap, items) ~t:(Int64.succ t) ~ans:(ans + 1)

let ans = solve (Heap.empty, Array.to_list items)

let () = printf "%d\n%!" ans
