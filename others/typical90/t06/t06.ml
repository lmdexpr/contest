open Core

let n, k = Scanf.scanf "%d %d" Tuple2.create

let s = Scanf.scanf " %s" ident

let c = Array.make_matrix ~dimx:(n+1) ~dimy:26 0

let atoi a = Char.(to_int a - to_int 'a')
let itoa i = Char.(to_int 'a' + i |> of_int_exn)

let () =
  for i = 0 to 25 do
    c.(n).(i) <- n
  done;

  for i = 1 to n do
    let i = n - i in

    for j = 0 to 25 do
      c.(i).(j) <-
        if atoi s.[i] = j then i
        else
          c.(i+1).(j)
    done
  done

let rec solve ?(i=1) ?(pos=0) f =
  if i > k then ()
  else
    let next_pos = Array.findi c.(pos) ~f:(fun _ pos -> n - pos - 1 + i >= k) in
    let i = i + 1 in
    let pos =
      match next_pos with 
      | None          -> pos
      | Some (j, pos) -> f @@ itoa j; pos + 1 
    in
    solve ~i ~pos f

let () =
  solve @@ printf "%c";
  Out_channel.(newline stdout)
