open List;;
let plateau lista = 
  match lista with
  | [] -> 0
  | h::t ->
      snd (fold_left (fun ((ileobecnie, jaki), ilemaks)  e -> if e = jaki then ((ileobecnie+1, jaki), max ilemaks (ileobecnie+1))
                       else ((1, e), max ilemaks (ileobecnie))) 
             ((h, 1), 1) t);;

plateau [1;2;2;1;1;1;3;3;4;4;4;4;2;2;2;3;3;3;5];;