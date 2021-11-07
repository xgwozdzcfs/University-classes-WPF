open List;;
let kompresuj lista = 
	 match lista with
	 | [] -> []
	 | [x] -> [x]
	 | h::t -> let (poprzedni, ile, lista)=
              fold_left (fun (poprzedni, ile, lista) x -> if x = poprzedni then (poprzedni, ile*2, lista) else (x, 1, (ile*(2*poprzedni-1))::lista))
                (h, 1, []) t in
      rev((ile*(2*poprzedni-1))::lista);;

kompresuj[1;2;2;3;3;3;4;5;5];;
kompresuj [1; 2; 2; 5; 11; 11; 2];;
