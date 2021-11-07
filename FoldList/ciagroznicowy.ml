open List;;
let ciagroznicowy lista=
  match lista with
  | [] -> []
  | h::t->
  let (a,l)= (fold_left (fun (poprzedni, l) e -> (e, (e-poprzedni)::l) ) (hd lista, []) (tl lista))
  in rev l;;
  
ciagroznicowy [1; 3; -2; 4; -1; 1];;
                                                
