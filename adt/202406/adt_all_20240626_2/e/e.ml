open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let c = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let n = min h w

let s = Array.create ~len:(n+1) 0

let size i j =
  [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]
  |> List.fold ~init:Int.max_value ~f:(fun acc (di, dj) ->
    let rec loop size =
      let i = i + size * di in
      let j = j + size * dj in
      if i < 0 || i >= h || j < 0 || j >= w || Char.(c.(i).(j) = '.') then size - 1
      else
        loop (size + 1)
    in
    min acc (loop 1)
  )

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if Char.(c.(i).(j) = '#') then
        let size = size i j in
        s.(size) <- s.(size) + 1
    done
  done

let () =
  for i = 1 to n do
    printf "%d " s.(i)
  done;
  printf "\n"
