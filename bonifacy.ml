open List;;

let bonifacy n lista = 
  let rec pom e1 e2 e3 lista nr = 
    if nr > n then e1
    else if hd lista = 0 
    then pom (e1+e2) e1 e2 (if (tl lista)=[] then lista else tl lista) (nr+1)
    else pom (e1+e3) e1 e2 (if (tl lista)=[] then lista else tl lista) (nr+1)
  in
  if n<3 then 
    if n=0 then 0 else 1
  else
    pom 1 1 0  ( tl (tl ( tl lista@lista@lista@lista))) 3;;

assert (map (fun i -> bonifacy i [1; 1; 0; 1]) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] = [0; 1; 1; 1; 2; 3; 5; 7; 10; 15; 25; 35; 50]);;
assert (map (fun i -> bonifacy i [0; 0; 0]) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55]);;
assert (map (fun i -> bonifacy i [1; 0; 0; 1]) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] = [0; 1; 1; 1; 2; 3; 5; 7; 10; 17; 27; 37; 54]);;