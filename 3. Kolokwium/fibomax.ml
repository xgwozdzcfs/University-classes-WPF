(*rozklad liczby na maksymalna liczbe liczb fibonacciego*)
open Array;;
open List;;
let fibomax n= 
  let fib = make (n+2) 0
  and k = ref n 
  and m = ref 2 in 
  fib.(1) <- 1;
  fib.(2) <- 2;
  for i = 3 to n do
    if fib.(i-1) + fib.(i-2) <= n then
      begin
        fib.(i) <- fib.(i-1) + fib.(i-2);
        m := i;
      end
    else 
      fib.(i) <- n+1;
  done;
  let rozkl = ref [] in
  let gdzie = make (!m +2)false in
  for i = !m downto 1 do
    if !k >= fib.(i) then
      begin
        k := !k - fib.(i);
        rozkl := i :: !rozkl;
        gdzie.(i) <- true;
      end
  done;
  rozkl := rev !rozkl; 
  let rec rozloz lista ak =
    match lista with
    | [] -> ak
    | h::t -> 
        if h = 0 then ak
        else    if h = 1 || h = 2 then 
          h::ak 
        else if gdzie.(h-1) = true then
          rozloz t (fib.(h)::ak)
        else 
          rozloz ((h-2)::t) (fib.(h-1)::ak)
  in
  rozloz !rozkl []
;;

fibomax 42;;
fibomax 25;;
