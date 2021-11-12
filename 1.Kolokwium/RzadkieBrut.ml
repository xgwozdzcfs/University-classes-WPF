open List;;



let rzadkie x =
  let rec pom l s =
    if l=x+1 then s else
      let rec oblicz l ak= 
        match l with 
        | 0 -> ak
        | x -> oblicz (x/2) ((x mod 2)::ak) in 
      let l1 = oblicz l [] in
      match l1 with 
      | [x] -> pom (l+1) s+1
      | h::t -> let rec pf lista pop = 
                  match lista with 
                  | [] -> true
                  | h::t -> if h = pop && pop = 1 then false else pf t h
          in if pf t h = true then pom (l+1) (s+1) else
            pom (l+1) s 
  in pom 1 0;;
  
rzadkie 42;;