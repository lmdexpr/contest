open Core

let n, k = Scanf.scanf "%d %d" Tuple2.create

let fact_count = Array.init (n+1) ~f:(const 0)

let () =
  for i = 2 to n do
    if fact_count.(i) = 0 then
      Iter.int_range_by i n ~step:i
      |> Iter.iter (fun j -> fact_count.(j) <- fact_count.(j) + 1)
  done

let () =
  Array.count fact_count ~f:((<=) k)
  |> printf "%d\n%!"
