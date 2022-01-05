open Array;;
open List;;
let prime x =
  let ans = ref true in 
  let i = ref 2 in
  while !i * !i <= x && !ans = true do
    if x mod !i = 0 then 
      ans := false;
    i := !i +1;
  done;
  !ans;; 

let ciag n = 
  let dp = make (n+1) 0 in
  dp.(2) <- 1;
  for i = 3 to n do
    if prime i = true then dp.(i) <- 1
    else
      begin
        dp.(i) <- dp.(i-1) + 1;
        let j = ref 2 in
        while !j * !j <= n do
          if n mod !j = 0 && prime (n/ (!j)) = true then
            dp.(i) <- min dp.(i) (1+dp.(!j)); 
          if n mod !j = 0 && prime !j = true then
            dp.(i) <- min dp.(i) (1+dp.(n/(!j))); 
          j := !j + 1;
        done;
      end
  done;
  dp.(n);; 

ciag 16;;
ciag 19;;
ciag 22;;
ciag 20;;
