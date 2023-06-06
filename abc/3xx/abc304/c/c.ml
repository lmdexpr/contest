open Core
open Scanf

let n, d = scanf "%d %d" Tuple2.create

let p = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let near = Array.init n ~f:(fun i ->
    Iter.(0 -- (n - 1))
    |> Iter.filter (fun j -> i <> j)
    |> Iter.filter (fun j ->
        let x1, y1 = p.(i) in
        let x2, y2 = p.(j) in
        let dx = x1 - x2 in
        let dy = y1 - y2 in
        dx * dx + dy * dy <= d * d
      )
  )

let affected = Array.init n ~f:(const false)
let () =
  affected.(0) <- true;
  let rec bfs q =
    match Fqueue.dequeue q with
    | None        -> ()
    | Some (i, q) ->
      near.(i)
      |> Iter.fold (fun q j ->
          if affected.(j) then q
          else (
            affected.(j) <- true;
            Fqueue.enqueue q j
          )
        )
        q
      |> bfs
  in
  bfs (Fqueue.singleton 0)

let () =
  for i = 0 to n - 1 do
    printf "%s\n" (if affected.(i) then "Yes" else "No")
  done
