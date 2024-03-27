open Core
open Scanf

let n = scanf "%d" Fn.id

let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)
let t = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let t = Array.make_matrix ~dimx:(n - 1 + n + n - 1) ~dimy:(n - 1 + n + n - 1) '.'
let () =
  for i = 1 to n do
    let s = scanf " %s" String.to_array in
    for j = 1 to n do
      t.(i + n - 1).(j + n - 1) <- s.(j - 1)
    done
  done

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let solve i j =
  let d = Iter.((0 -- (n - 1)) * (0 -- (n - 1))) in
  Iter.for_all Char.(fun (di, dj) -> s.(di).(dj) = t.(i + di).(j + dj)) d ||
  Iter.for_all Char.(fun (di, dj) -> s.(n - 1 - dj).(di) = t.(i + di).(j + dj)) d ||
  Iter.for_all Char.(fun (di, dj) -> s.(n - 1 - di).(n - 1 - dj) = t.(i + di).(j + dj)) d ||
  Iter.for_all Char.(fun (di, dj) -> s.(dj).(n - 1 - di) = t.(i + di).(j + dj)) d

let yes () = printf "Yes\n%!" ; exit 0

let () =
  for i = 0 to 2 * n - 2 do
    for j = 0 to 2 * n - 2 do
      if solve i j then yes ()
    done
  done

let () = printf "No\n%!"
