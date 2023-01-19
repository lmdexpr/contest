open Core

let () = Out_channel.(flush stdout)
let lines = In_channel.(input_lines stdin) |> List.to_array

let n, l = Scanf.sscanf lines.(0) "%d %d" @@ fun x y -> x, y

let pos = Array.init n ~f:ident

let () =
  for i = 1 to l do
    lines.(i)
    |> String.to_array
    |> Array.iteri ~f:(fun i -> function
        | '-' -> let i = i / 2 in Array.swap pos i (i + 1)
        | _   -> ()
      )
  done

let y =
  lines.(l + 1)
  |> fun s -> String.index_exn s 'o'
  |> fun i -> i / 2

let () = Printf.printf "%d\n%!" (pos.(y) + 1)
