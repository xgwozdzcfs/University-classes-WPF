open List;;
let bonifacy n lista = 
  if n = 0 then 0
  else if n = 1 || n = 2 then 1
  else 
    let rec pom nr lista1 xakt xpop xpopp =
      if nr = n+1 then  xakt
      else 
        match lista1 with
        | [] -> pom (nr+1) (tl lista) (if hd lista = 0 then xakt + xpop else xakt + xpopp) xakt xpop
        | h::t -> pom (nr+1) t (if h = 0 then xakt + xpop else xakt + xpopp) xakt xpop
    in 
    let poczlista = if  tl(tl(tl lista)) = [] then lista else tl(tl(tl lista)) in 
    pom 3 poczlista 1 1 0;;