open Core

let modulo = 1000000007

let n, b, k = Scanf.scanf "%d %d %d" Tuple3.create

let c = Array.init k ~f:(fun _ -> Scanf.scanf " %d" ident)

let dp = Array.make_matrix ~dimx:(n+1) ~dimy:b 0

let _digit_dp =
  dp.(0).(0) <- 1;
  for i = 0 to n - 1 do
    for j = 0 to b - 1 do
      Array.iter c ~f:(fun c ->
          let next_mod = (10 * j + c) % b in
          dp.(i+1).(next_mod) <- (dp.(i+1).(next_mod) + dp.(i).(j)) % modulo
        )
    done
  done

let () = printf "%d\n%!" dp.(n).(0)
