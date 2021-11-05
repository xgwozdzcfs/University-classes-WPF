open List;;
let prextrema lista=
  match lista with
  | [] -> []
  | h::t->
  let (a,b, l)= (fold_left (fun (mine, maxe, l) e -> if e < mine then (e, maxe, e::l) else if e > maxe then (mine, e, e::l) else (mine, maxe, l)) (hd lista, hd lista, [hd lista]) lista)
  in rev l;;
  
prextrema [-2; 1; 0; 1; 3; 2; -4; -3; -5];;
prextrema [-2; 1; 0; 1; 3; 2; -1; 5; 4; -3; 2; 1; 7]
                                                