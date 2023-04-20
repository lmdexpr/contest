(* https://atcoder.jp/contests/abc088/tasks/abc088_d *)
open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ ->
    scanf " %s" String.to_array |> Array.map ~f:Char.((=) '.')
  )

let bfs () =
  let dist = Array.init h ~f:(fun _ -> Array.create ~len:w @@ -1) in
  let rec bfs q = 
    match Fqueue.dequeue q with
    | None                                            -> -1
    | Some ((d, x, y), _) when x = w - 1 && y = h - 1 -> d
    | Some ((d, x, y), q) ->
      Iter.of_list [ (x+1, y); (x-1, y); (x, y+1); (x, y-1) ]
      |> Iter.filter (fun (x, y) -> 0 <= x && x < w && 0 <= y && y < h)
      |> Iter.filter (fun (x, y) -> s.(y).(x) && dist.(y).(x) = -1)
      |> Fn.flip Iter.fold q (fun q (x, y) ->
          if dist.(y).(x) = -1 then dist.(y).(x) <- d + 1;
          Fqueue.enqueue q (d + 1, x, y)
        )
      |> bfs
  in
  dist.(0).(0) <- 1;
  bfs @@ Fqueue.singleton (1, 0, 0)

let count = Array.map s ~f:(Array.count ~f:ident) |> Array.sum (module Int) ~f:ident

let ans = bfs ()
let ans = if ans = -1 then -1 else count - ans

let () = printf "%d\n%!" ans
