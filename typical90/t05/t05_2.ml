open Core

let modulo = 1000000007

module Matrix = struct
  let make = Array.make_matrix

  let identity n =
    let a = make ~dimx:n ~dimy:n 0 in
    for i = 0 to n - 1 do
      a.(i).(i) <- 1
    done;
    a
    
  let multiple ?(modulo=modulo) a b =
    let dimx = Array.length a
    and dimy = Array.length b.(0) in
    let c = make ~dimx ~dimy 0 in
    for i = 0 to dimx - 1 do
      for j = 0 to Array.length b - 1 do
        for k = 0 to dimy - 1 do
          c.(i).(k) <- (c.(i).(k) + a.(i).(j) * b.(j).(k)) % modulo
        done
      done
    done;
    c
  let ( * ) a b = multiple a b

  let power a t =
    let n = Array.length a in
    let e = Array.create ~len:64 (identity n) in
    e.(0) <- a;
    for i = 1 to 63 do
      e.(i) <- e.(i-1) * e.(i-1)
    done;
    Array.foldi e ~init:(identity n) ~f:(fun i acc e ->
        if Int64.(t land (1L lsl i) = 0L) then acc
        else
          acc * e
      )
end

let n, b, k = Scanf.scanf "%Ld %d %d" Tuple3.create

let c = Array.init k ~f:(fun _ -> Scanf.scanf " %d" ident)

let a = Matrix.make ~dimx:b ~dimy:b 0
let () =
  for j = 0 to b - 1 do
    Array.iter c ~f:(fun c ->
        let next_mod = (10 * j + c) % b in
        a.(j).(next_mod) <- a.(j).(next_mod) + 1
      )
  done

let z = Matrix.power a n

let () = printf "%d\n%!" z.(0).(0)
