open List;; 
let transpose lista =
  let rec f ak e = 
    match e with
    |[] -> ak 
    |h::t -> 
        match ak with
        | [] -> [h]::(f [] t)
        | p::q -> (h::p)::(f q t) 
  in 
  let poml = fold_left f [] lista in 
  map rev poml;;

transpose [[1;2]; [3;4]; [5;6]];;
transpose [[1; 2]; [5; 3]; [0; 1]];;
transpose [[1; 5; 0]; [2 ; 3; 1]] ;;
  