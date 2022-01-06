open List;;
open Array;;
open Set;;
module SI = Set.Make(Int);; 

let segment tablica = 
  let n = Array.length tablica in
  let dp = make_matrix (n+1) (n+1) (SI.empty) in
  for i = 1 to n do 
    dp.(i).(i) <- SI.add tablica.(i-1) dp.(i).(i); 
  done; 
  for ile = 2 to n do
    for i = 1 to n-ile+1 do
      let j = i + ile - 1 in
      for k = i to j-1 do 
        SI.iter ( fun e ->
            if SI.mem (e+1) dp.(k+1).(j) = true then
              dp.(i).(j) <- SI.add (e+2) dp.(i).(j);
            if SI.mem (e-1) dp.(k+1).(j) = true then
              dp.(i).(j) <- SI.add (e+1) dp.(i).(j);
          ) dp.(i).(k);
      done;
    done;
  done;
  let ans = ref 0 in
  for i = 1 to n do
    for j = 1 to n do
      SI.iter (fun e -> if e > !ans then ans := e) dp.(i).(j);
    done;
  done;
  !ans;;

segment [| 1; 3; 2; 3; 4; 5; 3; 6|];;
