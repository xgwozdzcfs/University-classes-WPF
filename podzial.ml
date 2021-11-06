open List;;
let podzial lista = 
  match lista with
  | [] -> []
  | [x] -> [[x]]
  | h::t ->
      let znak x = if x > 0 then 1 else if x < 0 then -1 else 0 in
  
      let (zn, lpom, a) = fold_left (fun (zn, lpom, a) e -> if znak e = zn then (zn, e::lpom, a) else (znak e, [e], (rev lpom)::a)) (znak h, [h], []) t in
      rev((rev lpom)::a);;

podzial [1;3;0;-2;-2;-4;9] = [[1; 3]; [0]; [-2;-2;-4]; [9]];;
