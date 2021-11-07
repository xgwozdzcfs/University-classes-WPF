open List;;
let usrednienie lista = 
  match lista with
  |[] -> []
  |[x] -> []
  | h::t -> let (a,k)=  ( fold_left (fun (poprzedni, ak) x -> (x, ((poprzedni+.x)/.2.)::ak)) (h, []) t) in
      rev k;;
usrednienie[1.0; 3.0; 4.0; 5.0; 7.0];;
