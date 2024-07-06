open Core
open Scanf

let n = scanf "%d" Fn.id

let s = scanf " %s" Fn.id
let t = scanf " %s" Fn.id

let bfs () =
  let w = String.count ~f:Char.((=) 'W') s in
  let b = String.count ~f:Char.((=) 'B') t in
  let got a = 
    Iter.(0 -- (n - 1)) |> Iter.for_all Char.(fun i -> a.(i) = t.[i]) 
  in
  let move a i =
    let a        = Array.copy a in
    let blank, _ = Array.findi_exn a ~f:Char.(fun _ -> (=) '.') in
    Array.swap a i blank;
    Array.swap a (i + 1) (blank + 1);
    a
  in
  let rec bfs queue =
    match Fqueue.dequeue queue with
    | None                        -> -1
    | Some ((c, a), _) when got a -> c
    | Some ((c, a), queue) ->
      Iter.(0 -- n)
      |> Iter.filter Char.(fun i -> a.(i) <> '.')
      |> Fn.flip Iter.fold queue
        (fun queue i -> Fqueue.enqueue queue (c + 1, move a i))
      |> bfs
  in
  if w + b <> n then -1
  else
    bfs (Fqueue.singleton
      (0, Array.init (n+2) ~f:(fun i -> if i >= n then '.' else s.[i]))
    )

let ans = bfs ()
let () = printf "%d\n%!" ans
