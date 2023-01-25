open Core

let n, k = Scanf.scanf "%d %d" Tuple2.create

let members =
  Array.init n ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)
  |> Iter.of_array

let max_a = Iter.map Tuple2.get1 members |> Iter.max_exn ~lt:(<)
and max_b = Iter.map Tuple2.get2 members |> Iter.max_exn ~lt:(<)

let max_a = max max_a k + 1
and max_b = max max_b k + 1

let sum = Array.make_matrix ~dimx:(max_a+1) ~dimy:(max_b+1) 0

let () =
  Iter.iter (fun (h, w) -> sum.(h).(w) <- sum.(h).(w) + 1) members;
  for i = 1 to max_a do
    for j = 1 to max_b do
      sum.(i).(j) <- sum.(i).(j) + sum.(i-1).(j)
    done
  done;
  for i = 1 to max_a do
    for j = 1 to max_b do
      sum.(i).(j) <- sum.(i).(j) + sum.(i).(j-1)
    done
  done

let (let+) x k = Iter.flat_map k x
and (let*) x k = Iter.map k x
let () =
  (
    let+ i = Iter.(0 -- (max_a - k - 1)) in
    let* j = Iter.(0 -- (max_b - k - 1)) in
    let w = i + k + 1 and h = j + k + 1 in
    sum.(w).(h) - sum.(i).(h) - sum.(w).(j) + sum.(i).(j)
  )
  |> Iter.max ~lt:(<)
  |> Option.value ~default:0
  |> printf "%d\n%!"
