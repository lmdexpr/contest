open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let s = Array.init n ~f:(fun _ ->
  scanf " %s" String.to_array
  |> Array.map ~f:(function '#' -> true | _ -> false)
)

let satisfy i j =
  s.(i  ).(j) && s.(i  ).(j+1) && s.(i  ).(j+2) && not s.(i  ).(j+3) &&
  s.(i+1).(j) && s.(i+1).(j+1) && s.(i+1).(j+2) && not s.(i+1).(j+3) &&
  s.(i+2).(j) && s.(i+2).(j+1) && s.(i+2).(j+2) && not s.(i+2).(j+3) &&
  not s.(i+3).(j) && not s.(i+3).(j+1) && not s.(i+3).(j+2) && not s.(i+3).(j+3) &&

  not s.(i+5).(j+5) && not s.(i+5).(j+6) && not s.(i+5).(j+7) && not s.(i+5).(j+8) &&
  not s.(i+6).(j+5) && s.(i+6).(j+6) && s.(i+6).(j+7) && s.(i+6).(j+8) &&
  not s.(i+7).(j+5) && s.(i+7).(j+6) && s.(i+7).(j+7) && s.(i+7).(j+8) &&
  not s.(i+8).(j+5) && s.(i+8).(j+6) && s.(i+8).(j+7) && s.(i+8).(j+8)

let () =
  for i = 0 to n - 9 do
    for j = 0 to m - 9 do
      if satisfy i j then
        printf "%d %d\n" (i+1) (j+1)
    done
  done
