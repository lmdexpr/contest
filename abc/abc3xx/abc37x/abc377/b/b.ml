open Core
open Scanf

let s = Array.init 8 ~f:(fun _ -> 
  scanf "%s\n" String.to_array |> Array.map ~f:(function '#' -> true | _ -> false)
)

let ans = Array.make_matrix ~dimx:8 ~dimy:8 false

let () =
  for i = 0 to 7 do
    for j = 0 to 7 do
      if s.(i).(j) then begin
        for k = 0 to 7 do
          ans.(i).(k) <- true
        done;
        for k = 0 to 7 do
          ans.(k).(j) <- true
        done;
      end
    done
  done

let ans = Array.fold ans ~init:0 ~f:(fun acc row -> acc + Array.count row ~f:not)

let () = printf "%d\n%!" ans
