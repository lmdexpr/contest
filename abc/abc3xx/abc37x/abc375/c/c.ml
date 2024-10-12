open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let ans = Array.make_matrix ~dimx:n ~dimy:n ' '
let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let d = min (min (i + 1) (j + 1)) (min (n - i) (n - j)) in
      let ni, nj =
        Iter.(1 -- d % 4)
        |> Iter.fold
          (fun (ni, nj) _ -> nj, n - 1 - ni)
          (i, j)
      in
      ans.(ni).(nj) <- a.(i).(j)
    done
  done

let () =
  Array.iter ans ~f:(fun a -> Array.iter a ~f:(printf "%c"); printf "\n")
