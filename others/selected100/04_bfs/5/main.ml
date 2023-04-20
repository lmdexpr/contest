(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1166&lang=jp *)
module Input = struct
  let lines _ = read_line ()

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

    let lines n _ = lines () |> split_on_char ~size:n ' '
  end
end

let input_w_h () =
  Input.Int.lines 2 ()
  |> function
  | [| w; h |] -> w, h
  | _          -> failwith "invalid input"

let bfs (w, h) wall =
  let dist = Array.init h (fun _ -> Array.make w 0) in
  let q = Queue.create () in

  Queue.push (1, 0, 0) q;
  while not (Queue.is_empty q) do
    let d, x, y = Queue.take q in
    if dist.(y).(x) = 0 || dist.(y).(x) > d then begin
      dist.(y).(x) <- d;
      [ (x+1, y); (x-1, y); (x, y+1); (x, y-1) ]
      |> List.iter (fun (tx, ty) ->
          if 0 <= tx && tx < w && 0 <= ty && ty < h && wall.(y+ty).[x+tx] = '0'
          then
            Queue.push (d+1, tx, ty) q
        );
    end
  done;

  dist.(h-1).(w-1)

let rec go solve (w, h) =
  if w <> 0 || h <> 0 then begin
    Array.init (2 * h - 1) Input.lines
    |> Array.mapi (fun i s -> if i mod 2 = 0 then s ^ "_" else s)
    |> solve (w, h)
    |> Printf.printf "%d\n";

    input_w_h ()
    |> go solve
  end

let () = input_w_h () |> go bfs
