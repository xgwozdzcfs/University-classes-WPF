open List;;
let widoczne lista = 
    let suma = fold_left (+) 0 lista in
    rev( snd( fold_left (fun a x -> if x = fst a && x = suma - 2*x then (fst a + x, x::(snd a)) else (fst a + x, snd a)) (0, []) lista ));;

widoczne [1;3;4;2;2];;