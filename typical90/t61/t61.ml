open Core

let q = Scanf.scanf "%d" ident

let query q = function
  | 1 -> Deque.enqueue_front q
  | 2 -> Deque.enqueue_back q
  | _ -> fun x ->
    x - 1 + Deque.front_index_exn q
    |> Deque.get q
    |> printf "%d\n%!" 

let () =
  let queue = Deque.create ~initial_length:q () in
  for _ = 1 to q do
    Scanf.scanf " %d %d" @@ query queue
  done
