open Core
open Scanf

let r, c = scanf "%d %d" Tuple2.create
let b = Array.init r ~f:(fun _ -> scanf " %s" String.to_array)

let char_to_digit c =
  let zero = Char.to_int '0' in
  let c    = Char.to_int c in
  let nine = Char.to_int '9' in
  Option.some_if (zero <= c && c <= nine) (c - zero)

let rec bomb i j p =
  if 0 <= i && i < r && 0 <= j && j < c && p >= 0 then begin
    if Option.is_none @@ char_to_digit b.(i).(j) then
      b.(i).(j) <- '.';
    bomb i (j + 1) (p - 1);
    bomb i (j - 1) (p - 1);
    bomb (i + 1) j (p - 1);
    bomb (i - 1) j (p - 1);
  end


let () = 
  for i = 0 to r - 1 do
    for j = 0 to c - 1 do
      match char_to_digit b.(i).(j) with
      | Some p -> b.(i).(j) <- '.'; bomb i j p
      | None   -> ()
    done
  done;
  Array.iter b ~f:(fun b -> Array.iter b ~f:(printf "%c"); printf "\n");
