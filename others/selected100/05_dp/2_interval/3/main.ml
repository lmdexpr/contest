(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1611&lang=jp *)
open Scanf
open Printf

let solve n w =
  let dp = Array.make_matrix n n 0 in
  for length = 2 to n do
    for start = 0 to n - length do
      let stop = start + length - 1 in

      if dp.(start + 1).(stop - 1) = length - 2
      && abs (w.(start) - w.(stop)) <= 1
      then
        dp.(start).(stop) <- length;

      let detour k = dp.(start).(k) + dp.(k+1).(stop) in
      let rec fold_max ?(i=start) acc ~f =
        if i >= stop then acc
        else
          fold_max ~i:(i + 1) ~f @@ max acc (f i)
      in
      dp.(start).(stop) <- fold_max dp.(start).(stop) ~f:detour
    done
  done;
  dp.(0).(n - 1)

let rec go n =
  if n <> 0 then begin
    let w = Array.init n (fun _ -> scanf " %d" @@ fun x -> x) in
    printf "%d\n%!" @@ solve n w;
    scanf " %d" go
  end

let () = scanf "%d" go
