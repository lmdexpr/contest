open Core

let n, r, d = Scanf.scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

(* combination using dp *)
let dp = Array.init (n+1) ~f:(fun _ -> Array.make_matrix ~dimx:(r+1) ~dimy:d (-1))
let () =
  dp.(0).(0).(0) <- 0;
  for i = 0 to n - 1 do
    for j = 0 to r do
      for k = 0 to d - 1 do
        let now = dp.(i).(j).(k) and next = dp.(i+1).(j) in

        if now <> -1 then begin
          next.(k) <- Int.max next.(k) now;

          if j <> r then
            let next = dp.(i+1).(j+1) and x = (k + a.(i)) % d in
            next.(x) <- Int.max next.(x) (now + a.(i))
        end
      done
    done
  done


let () = printf "%d\n%!" dp.(n).(r).(0)
