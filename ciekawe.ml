open List;;
let podzial lista = 
    let pom = fold_left (fun a x -> match fst a with 
                            | [] -> ([x], snd a)
                            | h::t -> if h = x then ([x], (rev(fst a))::(snd a))
                                    else (x::(fst a), snd a)  
                             ) ([], []) lista in
                             rev(rev(fst pom)::(snd pom));;
    

podzial [1;3;4;2;2];;
podzial [3;2;2;5;7;5;4;4;3;1]