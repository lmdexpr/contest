open Core

let n, x = Scanf.scanf "%d %d" Tuple2.create
let coins = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)

let dp = Array.make_matrix ~dimx:(n+1) ~dimy:(x+1) false
let () =
  dp.(0).(0) <- true;
  for i = 1 to n do
    let a, b = coins.(i - 1) in
    dp.(i).(0) <- true;
    for j = 0 to x do
      for k = 1 to b do
        if dp.(i - 1).(j) then begin
          dp.(i).(j) <- true;
          if j + a * k <= x then dp.(i).(j + a * k) <- true
        end
      done
    done
  done

let () = printf "%s\n%!" @@ if dp.(n).(x) then "Yes" else "No"
