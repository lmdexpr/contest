(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1160&lang=jp *)
module Input = struct
  module Int = struct
    let split_on_char ~size sep s =
      let r = Array.make size 0 in
      let p = ref (size - 1) in
      let j = ref (String.length s) in
      for i = String.length s - 1 downto 0 do
        if String.unsafe_get s i = sep then begin
          r.(!p) <- String.sub s (i + 1) (!j - i - 1) |> int_of_string;
          decr p;
          j := i
        end
      done;
      r.(0) <- String.sub s 0 !j |> int_of_string;
      r

    let lines n () = read_line () |> split_on_char ~size:n ' '
  end
end

let dfs w h map =
  let rec dfs x y = 
    map.(x).(y) <- 0;
    [ (x+1, y);   (x-1, y);   (x, y+1);   (x, y-1)
    ; (x+1, y+1); (x+1, y-1); (x-1, y+1); (x-1, y-1)
    ]
    |> List.iter (fun (x, y) ->
        if 0 <= x && x < h && 0 <= y && y < w && map.(x).(y) = 1 then
          dfs x y
      )
  in
  let count = ref 0 in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if map.(i).(j) = 1 then begin
        dfs i j;
        incr count
      end
    done
  done;
  !count

let () =
  while true do
    match Input.Int.lines 2 () with
    | [| 0; 0 |] -> exit 0
    | [| w; h |] ->
      let map = Array.init h (fun _ -> Array.make w 0) in
      for i = 0 to h-1 do
        map.(i) <- Input.Int.lines w ()
      done;
      Printf.printf "%d\n" @@ dfs w h map
    | _ -> ()
  done
