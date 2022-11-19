open Core

let height, width, n, h, w = Scanf.scanf "%d %d %d %d %d" @@ fun a b c d e -> a, b, c, d, e 

let minx = Array.create ~len:n @@ height + 1
let maxx = Array.create ~len:n 0
let miny = Array.create ~len:n @@ width + 1
let maxy = Array.create ~len:n 0

let () =
  for i = 1 to height do
    for j = 1 to width do
      let a = Scanf.scanf " %d" @@ fun x -> x - 1 in

      minx.(a) <- min minx.(a) i;
      maxx.(a) <- max maxx.(a) i;

      miny.(a) <- min miny.(a) j;
      maxy.(a) <- max maxy.(a) j;
    done
  done

let () =
  for k = 0 to height - h do
    for l = 0 to width - w do
      let f a =
        k < minx.(a) && maxx.(a) <= k + h &&
        l < miny.(a) && maxy.(a) <= l + w
      in
      let f a = f a |> Bool.to_int in
      let rec solve a acc =
        if a < n then solve (a + 1) (acc - f a)
        else
          acc
      in
      printf "%d " @@ solve 0 n 
    done;

    Out_channel.(newline stdout)
  done
