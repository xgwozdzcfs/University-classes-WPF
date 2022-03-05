open List;;
open Array;;
let energia k tablica e =
  let dp = Array.make_matrix (k+2) (e+2) (-1) in
  dp.(0).(0) <- 0;
  for i = 1 to k do 
    for it = Array.length tablica - 1 downto 0 do
      for j = 0 to e - tablica.(it) do 
        if dp.(i-1).(j) >= 0 && dp.(i-1).(j) < it+1  then
          dp.(i).(j+tablica.(it)) <-  it+1;
      done;
    done;
  done;
  if dp.(k).(e) = -1 then [||]
  else 
    let ans = ref [] in
    let sum = ref e in
    for i = k downto 1 do
      ans := (dp.(i).(!sum)-1) :: !ans;
      sum := !sum - tablica.(dp.(i).(!sum)-1);
    done;
    Array.of_list !ans;;

energia 4 [|2; 8; 12; 12; 20|] 42;;
energia 4 [|2; 8; 12; 12; 20|] 60;;
