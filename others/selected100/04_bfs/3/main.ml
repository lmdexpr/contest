(* https://atcoder.jp/contests/joi2011yo/tasks/joi2011yo_e *)
open Core
open Scanf

let h, w, n = scanf "%d %d %d" Tuple3.create

let map = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let sx, sy =
  Array.find_mapi_exn map ~f:(fun i map ->
      Array.find_mapi map ~f:(fun j -> function
          | 'S' -> Some (j, i)
          | _   -> None
        )
    )

let map =
  Array.map map ~f:(Array.map ~f:(function
      | c when Char.is_digit c -> Char.to_int c - Char.to_int '0'
      | '.' | 'S' -> 0
      | _         -> 10
    ))

let bfs (acc, sx, sy) target =
  let dst = Array.init h ~f:(fun _ -> Array.create ~len:w (-1)) in dst.(sy).(sx) <- 0;
  let rec bfs q = 
    match Fqueue.dequeue q with
    | None                                          -> assert false
    | Some ((d, x, y), _) when map.(y).(x) = target -> acc + d, x, y
    | Some ((d, x, y), q) ->
      Iter.of_list [ (x+1, y); (x-1, y); (x, y+1); (x, y-1) ]
      |> Iter.filter (fun (x, y) ->
          0 <= x && x < w && 0 <= y && y < h &&
          map.(y).(x) < 10 &&
          dst.(y).(x) = -1
        )
      |> Fn.flip Iter.fold q (fun q (x, y) ->
          let d = d + 1 in
          if dst.(y).(x) = -1 then dst.(y).(x) <- d;
          Fqueue.enqueue q (d, x, y)
        )
      |> bfs
  in
  bfs @@ Fqueue.singleton (0, sx, sy)

let ans =
  Iter.(1 -- n)
  |> Iter.fold bfs (0, sx, sy) 
  |> Tuple3.get1

let () = printf "%d\n%!" ans
