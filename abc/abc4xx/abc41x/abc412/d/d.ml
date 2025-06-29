open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let deg = Array.create ~len:n 0

let es = Array.init m ~f:(fun _ ->
  scanf " %d %d" @@ fun a b -> a - 1, b - 1
)

let () =
  Array.iter es ~f:(fun (a, b) ->
    deg.(a) <- deg.(a) + 1;
    deg.(b) <- deg.(b) + 1;
  )

let deg_i = Array.mapi deg ~f:(fun i d -> d, i)
let ans = ref 0
let () =
  Array.sort deg_i ~compare:(fun (d1, _) (d2, _) -> Int.descending d1 d2);
  Array.iter deg_i ~f:(fun (d, i) ->
    if 2 < d then (
      let filtered = Array.filter_mapi es ~f:(fun i (a, b) -> 
        Option.some_if (a = i || b = i) (a, b, i)
      ) in
      Array.sort filtered ~compare:(fun (a1, b1, _) (a2, b2, _) ->
        let x = if a1 = i then b1 else a1 in
        let y = if a2 = i then b2 else a2 in
        Int.descending deg.(x) deg.(y)
      );
      Array.iter filtered ~f:(fun (a, b, i) ->
        if 2 < deg.(a) || 2 < deg.(b) then (
          es.(i) <- (-1, -1);
          deg.(a) <- deg.(a) - 1;
          deg.(b) <- deg.(b) - 1;
          incr ans;
        )
      )
    )
  )

let ans =
  let zero_count = Array.count deg ~f:(fun d -> d = 0) in
  let one_count  = Array.count deg ~f:(fun d -> d = 1) in
  !ans
  + if zero_count = 1 then 3 else zero_count
  + one_count / 2 (* deg = 1 は必ず偶数個になる *)

let () = printf "%d\n%!" ans
