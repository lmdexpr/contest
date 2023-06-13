open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let cumsum = Array.init n ~f:(fun i ->
    if i = 0 || i % 2 = 1 then 0 else a.(i) - a.(i - 1)
  )
let () =
  for i = 1 to n - 1 do
    cumsum.(i) <- cumsum.(i) + cumsum.(i - 1);
  done

let sleep_until t =
  let i = Array.binary_search a ~compare `Last_less_than_or_equal_to t in
  let i = Option.value_exn i in
  cumsum.(i) +
  if i % 2 = 0 then 0 else t - a.(i)

let solve l r = sleep_until r - sleep_until l
let solve (l, r) = printf "%d\n" @@ solve l r

let () =
  scanf " %d" ident
  |> Array.init ~f:(fun _ -> scanf " %d %d" Tuple2.create)
  |> Array.iter ~f:solve
