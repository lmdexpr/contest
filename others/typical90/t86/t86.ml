open Core

let n, q = Scanf.scanf "%d %d" Tuple2.create

let x = Array.init q ~f:(const 0)
let y = Array.init q ~f:(const 0)
let z = Array.init q ~f:(const 0)
let w = Array.init q ~f:(const 0L)

let () =
  for i = 0 to q - 1 do
    Scanf.scanf " %d %d %d %Ld" @@ fun xi yi zi wi ->
    x.(i) <- xi;
    y.(i) <- yi;
    z.(i) <- zi;
    w.(i) <- wi
  done

let ways i =
  Iter.(0 -- ((1 lsl n) - 1))
  |> Iter.filter_count (fun mask ->
      let bit = Array.init (n+1) ~f:(fun j -> (mask / (1 lsl j)) % 2) in
      Iter.(0 -- (q - 1))
      |> Iter.for_all (fun j ->
          let x = x.(j) - 1 and y = y.(j) - 1 and z = z.(j) - 1 in
          let xyz = bit.(x) lor bit.(y) lor bit.(z) in
          Int64.(of_int xyz = w.(j) / (1L lsl i) % 2L)
        )
    )

let modulo = 1000000007
let( *%) a b = a * b % modulo

let () =
  Iter.(0 -- 59)
  |> Iter.map ways
  |> Iter.fold (fun ans way -> ans *% way) 1
  |> printf "%d\n%!"
