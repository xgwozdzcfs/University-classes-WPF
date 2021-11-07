open List;;
let zlecenia lista =
    let listapom = rev(fold_left (fun a (p,q) -> match a with
                                | [] -> (p+q, p)::a 
                                | h::t -> let pocz = max (fst h) p in (pocz+q, p)::a
                ) [] lista ) in
    rev(fold_left (fun a (p,q) -> (p - q)::a) [] listapom);;  
    

zlecenia [(-1, 1); (2, 2); (3, 3); (4, 2); (10, 2)];;
