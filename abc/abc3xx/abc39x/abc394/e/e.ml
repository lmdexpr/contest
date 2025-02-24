open Core
open Scanf

let n = scanf " %d" Fn.id
let c = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let inf = 5_000_000

let ans = Array.make_matrix ~dimx:n ~dimy:n inf
let q   = Queue.create ()

let () =
  for i = 0 to n - 1 do
    ans.(i).(i) <- 0;
    Queue.enqueue q (i, i)
  done

let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i = j || Char.(c.(i).(j) = '-') then ()
      else (
        ans.(i).(j) <- 1;
        Queue.enqueue q (i, j)
      )
    done
  done

let () =
  while not (Queue.is_empty q) do
    let i, j = Queue.dequeue_exn q in
    for k = 0 to n - 1 do
      for l = 0 to n - 1 do
        if 
          Char.(c.(k).(i) <> '-' && c.(j).(l) <> '-' && c.(k).(i) = c.(j).(l)) 
          && ans.(k).(l) = inf
        then (
          ans.(k).(l) <- ans.(i).(j) + 2;
          Queue.enqueue q (k, l)
        )
      done
    done
  done

let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if ans.(i).(j) = inf then
        printf "-1 "
      else
        printf "%d " ans.(i).(j)
    done;
    printf "\n%!"
  done
