open Core
open Scanf

let n, x = scanf " %d %d" Tuple2.create

let foods = Array.init n ~f:(fun _ -> scanf " %d %d %d" Tuple3.create)

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(x + 1) (0, 0, 0)

let () = 
  let zero_count (a, b, c) = 
    Bool.to_int (a = 0) + Bool.to_int (b = 0) + Bool.to_int (c = 0)
  in
  let min_not_zero (a, b, c) =
    let min_not_zero a b =
      match a, b with
      | 0, 0 -> None
      | 0, _ -> Some b
      | _, 0 -> Some a
      | _    -> Some (min a b)
    in
    min_not_zero b c |> Option.bind ~f:(min_not_zero a)
  in
  let max v w =
    let zv = zero_count v and zw = zero_count w in
    if zw < zv then w
    else if zv < zw then v
    else
      match min_not_zero v, min_not_zero w with
      | Some k, Some l -> if l < k then v else w
      | Some _, None   -> v
      | _              -> w
  in
  for i = 0 to n - 1 do
    let v, a, calory = foods.(i) in
    for c = 0 to x do
      dp.(i + 1).(c) <-
      if c < calory then dp.(i).(c)
      else
        let q1, q2, q3 = dp.(i).(c - calory) in
        match v with
        | 1 -> max dp.(i).(c) (q1 + a, q2, q3)
        | 2 -> max dp.(i).(c) (q1, q2 + a, q3)
        | 3 -> max dp.(i).(c) (q1, q2, q3 + a)
        | _ -> assert false
    done
  done

let ans = 
  let a, b, c = dp.(n).(x) in
  min a (min b c)

let () = printf "%d\n%!" ans
