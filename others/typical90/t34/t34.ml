open Core

let n, k = Scanf.scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let memo = Hashtbl.create ~size:n (module Int)
let memo_get i = Hashtbl.find memo a.(i) |> Option.value ~default:0

let rec syakutori ~ans ~l ~r ~kind =
  if r >= n then ans
  else
    let l, r, kind =
      if kind < k || memo_get r > 0 then
        (Hashtbl.incr memo a.(r); l, r + 1, kind + Bool.to_int (memo_get r = 1))
      else
        (Hashtbl.decr memo a.(l); l + 1, r, kind - Bool.to_int (memo_get l = 0))
    in
    syakutori ~l ~r ~kind ~ans:(max ans (r - l))

let () =
  Hashtbl.incr memo a.(0);
  printf "%d\n%!" @@ syakutori ~l:0 ~r:1 ~kind:1 ~ans:(-1)
