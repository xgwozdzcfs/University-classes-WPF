let rozklady n =
  let rec fibonacci n ak l1 l2= 
    if n <= l1 then
      ak
    else 
      fibonacci n ((l1+l2)::ak) (l1+l2) l1
  in
  let dp = Array.make (n+1) 0 in
  dp.(0) <- 1; 
  List.iter (fun e ->
      for i = n downto 0 do
        if i + e <= n then
          dp.(i+e) <- dp.(i+e) + dp.(i);
      done;) (fibonacci n [2; 1] 2 1);
  dp.(n)
;;

rozklady 42;;
