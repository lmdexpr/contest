open Core
open Scanf

let n = scanf " %d" Fn.id

let k =
  try
    Iter.(1 -- (n + 4))
    |> Iter.find_map (fun i ->
      if i * (i - 1) / 2 = n then Some i else None
    )
    |> Option.value_exn
  with _ ->
    printf "No\n%!";
    exit 0

let ans = Array.init k ~f:(fun _ -> Int.Set.empty)

let () =
  let cnt = ref 0 in
  for i = 0 to k - 1 do
    for j = i + 1 to k - 1 do
      incr cnt;
      ans.(i) <- Set.add ans.(i) !cnt;
      ans.(j) <- Set.add ans.(j) !cnt;
    done
  done

let () =
  printf "Yes\n%!";
  printf "%d\n%!" k;
  Array.iter ans ~f:(fun ans ->
    printf "%d " (Set.length ans);
    Set.iter ans ~f:(fun v -> printf "%d " v);
    printf "\n%!";
  );
