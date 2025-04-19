open Core
open Scanf

let q = scanf " %d" Fn.id

let () =
  let queue = Queue.create () in
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 -> 
      let x = scanf " %d" Fn.id in
      Queue.enqueue queue x
    | _ ->
      printf "%d\n" @@ Queue.dequeue_exn queue
  done
