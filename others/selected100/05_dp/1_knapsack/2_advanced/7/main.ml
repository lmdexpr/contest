(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2199&lang=jp *)
open Scanf
open Printf

let solve n m =
  let c = Array.init m (fun _ -> scanf " %d" @@ fun x -> x) in
  let x = Array.init n (fun _ -> scanf " %d" @@ fun x -> x) in

  let dp = Array.init (n + 1) (fun _ -> Array.init 256 (fun _ -> max_int)) in
  dp.(0).(128) <- 0;

  for i = 0 to n - 1 do
    for j = 0 to 255 do
      let target = dp.(i).(j) in
      let next   = dp.(i + 1) in
      if target <> max_int then
        c |> Array.iter (fun c ->
            let y = j + c in
            let y = max 0 (min y 255) in
            let d = x.(i) - y in
            next.(y) <- min next.(y) (target + d * d)
          )
    done
  done;

  printf "%d\n%!" @@ Array.fold_left min max_int dp.(n)

let rec go n m =
  if n <> 0 && m <> 0 then begin
    solve n m;
    scanf " %d %d" go
  end
let () = scanf "%d %d" go
