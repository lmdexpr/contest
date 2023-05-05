(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_10_B&lang=ja *)
open Scanf
open Printf

let n = scanf "%d" (fun x -> x)
let m = Array.init n (fun _ -> scanf " %d %d" @@ fun x y -> x, y)

let chain = Array.make (n+1) 0
let () =
  Array.iteri (fun i (x, _) -> chain.(i) <- x) m;
  chain.(n) <- snd m.(n - 1)

let dp = Array.make_matrix (n + 1) (n + 1) 0
let () =
  for length = 2 to n do
    for start = 1 to n - length + 1 do
      let stop = start + length - 1 in

      let detour k = dp.(start).(k) + dp.(k+1).(stop) in
      let chains k = chain.(start - 1) * chain.(k) * chain.(stop) in

      let rec fold_min ?(i=start) acc ~f =
        if i >= stop then acc
        else
          fold_min ~i:(i + 1) ~f @@ min acc (f i)
      in
      dp.(start).(stop) <- fold_min max_int ~f:(fun i -> detour i + chains i)
    done
  done

let () = printf "%d\n%!" dp.(1).(n)
