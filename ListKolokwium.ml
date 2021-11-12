open List;;
let fit x lista =
  let rec lsum list ak =
    match list with
    | [] -> rev ak
    | h::t -> lsum t ((h+x)::ak) 
  in 
  let lists = lsum lista [] in
  let rec wyn1 lsuma l ak =
    if lsuma = [] then ak else
      match l with
      |[] -> ak
      |h::t -> if h + (hd lsuma) >= 0 then wyn1 (tl lsuma) l (min ak (abs(h + hd lsuma))) 
          else wyn1 lsuma t ak 
              
  in let cal1 = wyn1 (rev lists) (lista) (abs(x+hd lista + hd lista)) in
  let rec wyn1 lsuma l ak =
    if lsuma = [] then ak else
      match l with
      |[] -> ak
      |h::t -> if h + (hd lsuma) <= 0 then wyn1 (tl lsuma) l (min ak (abs(h + hd lsuma))) 
          else wyn1 lsuma t ak 
              
  in let cal2 = wyn1 (lists) (rev lista) (abs(x+hd lista + hd lista)) in 
  min cal2 cal1;;
          
          

fit 42 [-28; -25; -15; -4; 5; 6; 17];;
fit 5 [-1; 2; 3; 5; 6];;