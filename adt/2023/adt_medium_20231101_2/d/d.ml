open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let correct =
        let open Char in
        match a.(i).(j) with
        | 'W' -> a.(j).(i) = 'L'
        | 'L' -> a.(j).(i) = 'W'
        | 'D' -> a.(j).(i) = 'D'
        | _   -> true
      in
      if not correct then
        (printf "incorrect\n%!"; exit 0)
    done
  done

let () = printf "correct\n%!"
