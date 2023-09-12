open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let input s = s |> String.to_array |> Array.map ~f:(function '.' -> 0 | _ -> 1)

let a = Array.init n ~f:(fun _ -> scanf " %s" input)
let b = Array.init m ~f:(fun _ -> scanf " %s" input)

let contain offset_x offset_y =
  let rec loop = function
    | -1, _ | _, -1 -> true
    | i, j ->
      if a.(offset_x + i).(offset_y + j) = b.(i).(j) then
        let next = if j = 0 then i - 1, m - 1 else i, j - 1 in
        loop next
      else
        false
  in
  loop (m - 1, m - 1)

let () =
  for x = 0 to n - m do
    for y = 0 to n - m do
      if contain x y then begin
        printf "Yes\n%!";
        exit 0
      end
    done
  done

let () = printf "No\n%!"
