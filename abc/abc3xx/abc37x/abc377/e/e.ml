open Core
open Scanf

let power a b modulo =
  let open Int64 in
  Array.init num_bits ~f:Fn.id
  |> Array.fold ~init:(one, a) ~f:(fun (p, q) i ->
      if b land (one lsl i) <> zero then p * q % modulo, q * q % modulo
      else
        p, q * q % modulo
    )
  |> Tuple2.get1

let n, k = scanf "%d %Ld" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" @@ fun x -> x - 1)

let ans = Array.create ~len:n 0

let () =
  let used = Array.create ~len:n false in
  let rec solve i acc =
    if used.(i) then Array.of_list @@ List.rev acc
    else begin
      used.(i) <- true;
      solve a.(i) (i :: acc)
    end
  in
  for i = 0 to n - 1 do
    if not used.(i) then 
    let cycle = solve i [] in
    let cycle_len = Array.length cycle in
    let shift = power 2L k Int64.(of_int cycle_len) |> Int64.to_int_exn in
    for j = 0 to cycle_len - 1 do
      ans.(cycle.(j)) <- cycle.((j + shift) mod cycle_len)
    done
  done

let () =
  Array.iter ans ~f:(fun x -> printf "%d " @@ x + 1); printf "\n"
