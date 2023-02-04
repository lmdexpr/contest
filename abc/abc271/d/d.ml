open Core
open Scanf

let n, s = scanf "%d %d" Tuple2.create
let card = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dp = Array.init (n+1) ~f:(fun _ -> Array.init 10001 ~f:(const false))

let () =
  dp.(0).(0) <- true;
  Array.iteri card ~f:(fun i (a, b) ->
      for j = 10000 - max a b downto 0 do
        if dp.(i).(j) then begin
          dp.(i+1).(j + a) <- true;
          dp.(i+1).(j + b) <- true
        end
      done
    )

let reconstruct () =
  Iter.(n - 1 --^ 0)
  |> Iter.fold
    (fun (j, acc) i ->
       let a, b = card.(i) in
       let choice x y = if j - a >= 0 && dp.(i).(j - a) then x else y in
       j - choice a b, choice 'H' 'T' :: acc
    )
    (s, [])
  |> Tuple2.get2

let () =
  if not dp.(n).(s) then printf "No\n%!"
  else begin
    printf "Yes\n";
    List.iter (reconstruct ()) ~f:(printf "%c");
    printf "\n%!"
  end
