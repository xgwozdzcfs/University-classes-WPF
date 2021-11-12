open List;;
let dolki lista =
  match lista with 
  | [] -> 0
  | h::t ->
      let prefmax = rev( fold_left (fun (m::r) x -> ( (max x m)::m::r)) [h] t ) 
      in let revlista = rev lista 
      in let sufmax = fold_left (fun (m::r) x -> ( (max x m)::m::r)) [hd revlista] (tl revlista) in
      let jakimaks = fold_left2 (fun a x1 x2 -> (min x1 x2)::a) [] prefmax sufmax in
      fold_left2 (fun a x1 x2 -> max a (x2-x1)) 0 jakimaks lista;;
                  
                  
                  
dolki [7;2;6;4;5];;
dolki [2;2;2;2;2];;