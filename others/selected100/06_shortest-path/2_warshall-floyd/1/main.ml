(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_1_C&lang=ja *)
open Scanf
open Printf

let inf = Int64.(shift_left 1L 60)
let v, e = scanf "%d %d" @@ fun v e -> v, e

let dist = Array.make_matrix v v inf
let () =
  for _ = 1 to e do
    scanf " %d %d %Ld" @@ fun s t d ->
    dist.(s).(t) <- d
  done;
  for i = 0 to v - 1 do
    dist.(i).(i) <- 0L
  done

let _warshall_floyd =
  for k = 0 to v - 1 do
    for i = 0 to v - 1 do
      for j = 0 to v - 1 do
        let open Int64 in
        if dist.(i).(k) < inf && dist.(k).(j) < inf then
          dist.(i).(j) <- min dist.(i).(j) (add dist.(i).(k) dist.(k).(j))
      done
    done
  done;
  for i = 0 to v - 1 do
    if dist.(i).(i) < 0L then begin
      printf "NEGATIVE CYCLE\n"; exit 0
    end
  done

let () =
  for i = 0 to v - 1 do
    for j = 0 to v - 1 do
      if dist.(i).(j) = inf then
        printf "INF"
      else
        printf "%Ld" dist.(i).(j);

      if j < v - 1 then printf " "
    done;
    printf "\n"
  done
