open Core
open Scanf

let h, w = scanf " %d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let ly, lx, uy, ux =
  let lower_y = ref (h-1) in
  let lower_x = ref (w-1) in
  let upper_y = ref 0 in
  let upper_x = ref 0 in
  Array.iteri s ~f:(fun i ->
    Array.iteri ~f:(fun j -> function
      | '#' ->
        lower_y := min !lower_y i;
        lower_x := min !lower_x j;
        upper_y := max !upper_y i;
        upper_x := max !upper_x j;
      | _ -> ()
    )
  );
  !lower_y, !lower_x, !upper_y, !upper_x

let yes =
  Iter.(product (ly -- uy) (lx -- ux))
  |> Iter.for_all Char.(fun (y, x) -> s.(y).(x) <> '.')

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
