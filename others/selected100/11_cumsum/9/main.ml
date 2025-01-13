open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let imos = Array.make_matrix ~dimx:(n+3) ~dimy:(n+3) 0

let () =
  List.init m ~f:(fun _ -> scanf " %d %d %d" Tuple3.create)
  |> List.iter ~f:(fun (a, b, x) ->
    imos.(a).(b)         <- succ imos.(a).(b);
    imos.(a).(b+1)       <- pred imos.(a).(b+1);
    imos.(a+x+1).(b)     <- pred imos.(a+x+1).(b);
    imos.(a+x+1).(b+x+2) <- succ imos.(a+x+1).(b+x+2);
    imos.(a+x+2).(b+1)   <- succ imos.(a+x+2).(b+1);
    imos.(a+x+2).(b+x+2) <- pred imos.(a+x+2).(b+x+2)
  )

let () =
  for i = 0 to n + 2 do
    for j = 1 to n + 2 do
      imos.(i).(j) <- imos.(i).(j) + imos.(i).(j-1)
    done
  done;
  for i = 1 to n + 2 do
    for j = 0 to n + 2 do
      imos.(i).(j) <- imos.(i).(j) + imos.(i-1).(j)
    done
  done;
  for i = 1 to n + 2 do
    for j = 1 to n + 2 do
      imos.(j).(i) <- imos.(j).(i) + imos.(j-1).(i-1)
    done
  done

let ans = 
  Array.fold imos ~init:0 ~f:(fun acc row ->
    acc + Array.count row ~f:(fun x -> 0 < x)
  )

let () = printf "%d\n%!" ans
