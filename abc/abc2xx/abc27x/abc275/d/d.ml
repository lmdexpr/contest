open Core

let n = Scanf.scanf "%d" Fn.id

let memo = Hashtbl.create ~size:330 (module Int)

let rec f = function
| 0 -> 1
| n ->
  let find_or_add = Hashtbl.find_or_add memo in
  let m = n / 2
  and k = n / 3 in
  let a = find_or_add m ~default:(fun _ -> f m)
  and b = find_or_add k ~default:(fun _ -> f k) in
  a + b

let () = Printf.printf "%d\n" @@ f n
