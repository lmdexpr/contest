open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let s = scanf " %s" String.to_array

let mex i j k =
  let rec loop t = if t = i || t = j || t = k then loop (t + 1) else t in
  Int64.of_int @@ loop 0

let cumsum_m = Array.make_matrix ~dimx:(n + 1) ~dimy:3 0L
let () =
  for i = 0 to n - 1 do
    cumsum_m.(i + 1) <- Array.copy cumsum_m.(i);
    if Char.(s.(i) = 'M') then
      cumsum_m.(i + 1).(a.(i)) <- Int64.(+) cumsum_m.(i + 1).(a.(i)) 1L
  done
let cumsum_x = Array.make_matrix ~dimx:(n + 1) ~dimy:3 0L
let () =
  for i = n - 1 downto 0 do
    cumsum_x.(i) <- Array.copy cumsum_x.(i + 1);
    if Char.(s.(i) = 'X') then
      cumsum_x.(i).(a.(i)) <- Int64.(+) cumsum_x.(i).(a.(i)) 1L
  done

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.filter Char.(fun j -> s.(j) = 'E')
  |> Iter.map (fun j ->
      Array.foldi cumsum_m.(j) ~init:0L ~f:(fun i acc m ->
        Array.foldi cumsum_x.(j+1) ~init:acc ~f:Int64.(fun k acc x ->
          acc + m * x * mex i a.(j) k
        )
      )
    )
  |> Iter.fold Int64.(+) 0L

let () = printf "%Ld\n%!" ans
