open List;;
let rzadkie x=
  let rec oblicz l ak= 
    match l with 
    | 0 -> ak
    | x -> oblicz (x/2) ((x mod 2)::ak) in 
  let liczba = oblicz x [] in
  let rec oblicz2 liczba ak pop pf =
    match liczba with
    | [] -> ak
    | h::t -> if h = 1 && pop = 1 then oblicz2 t (0::ak) 0 true
        else if pf = true then oblicz2 t ((1-pop)::ak) (1-pop) true
        else oblicz2 t (h::ak) h false
  in let liczba = oblicz2 (tl liczba) [hd liczba] (hd liczba) false in 
  
  let rec fold lista pop ileakt ilepop popp=
    match lista with
    | [] -> pop 
    | h::t -> 
        if h = 1 then fold t (1+popp+pop+ilepop) (ilepop+1) (ileakt+ilepop) (-1)
        else fold t pop (ilepop+1) (ileakt+ilepop) 0
  in fold liczba 0 1 0 0 ;;
