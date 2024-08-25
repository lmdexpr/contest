open Core
open Scanf

let n = scanf "%d" Fn.id

let ans = Array.init 7 ~f:(fun _ -> [| [| |] |] )

let () =
  ans.(0) <- [| [| true |] |];
  for level = 1 to 6 do
    let size = Int.(3 ** level) in
    let ans_level = Array.make_matrix ~dimx:size ~dimy:size false in

    let prev = ans.(level - 1) in

    let w = Int.(3 ** (level - 1)) in

    for adj_i = 0 to 2 do
      for adj_j = 0 to 2 do
        for i = 0 to w - 1 do
          for j = 0 to w - 1 do
            ans_level.(adj_i * w + i).(adj_j * w + j) <- prev.(i).(j)
          done
        done
      done
    done;

    for i = 1 to w do
      for j = 1 to w do
        ans_level.(w + i - 1).(w + j - 1) <- false
      done
    done;

    ans.(level) <- ans_level;
  done

let () =
  Array.iter ans.(n) ~f:(fun ans ->
    Array.iter ans ~f:(fun b ->
      printf "%c" (if b then '#' else '.')
    );
    printf "\n"
  )
