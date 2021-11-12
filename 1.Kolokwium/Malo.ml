open List;;
let malo lista = 
  let rec dodatni lista ak= 
    match lista with 
    | [] -> rev ak
    | h::t -> if h>=0 then dodatni t (h::ak) else dodatni t ak in
  let rec ujemn lista ak =
    match lista with
    | [] -> rev ak
    | h::t -> if h<0 then ujemn t (h::ak) else ujemn t ak in
  let dodatnia = dodatni lista [] 
  and ujemna = ujemn lista [] in 
  if dodatnia = [] then let odwr = rev ujemna in abs(hd odwr + hd(tl odwr)) 
  else if ujemna = [] then abs(hd dodatnia + hd (tl dodatnia))
  else
    let rec wyn1 dod uj ak =
      if dod = [] then ak  else 
        match uj with
        | [] -> ak
        | h::t -> if h + hd dod >= 0 then wyn1 (tl dod) uj (min ak (abs(h + hd dod)))
            else wyn1 dod t (min ak (abs(h + hd dod))) in
    let cal1 = wyn1 (rev dodatnia) ujemna max_int in
    let rec wyn2 dod uj ak =
      if dod = [] then ak  else 
        match uj with
        | [] -> ak
        | h::t -> if h + hd dod <= 0 then wyn1 (tl dod) uj (min ak (abs(h + hd dod)))
            else wyn2 dod t (min ak (abs(h + hd dod))) in
    let cal2 = wyn2 dodatnia (rev ujemna) cal1 in
    let ujemna = rev ujemna in
    let cal2=
      match ujemna with
      |[x]-> cal2 
      | h::j::t -> min cal2 (abs(h+j))
    in
    let cal2 =
      match dodatnia with
      |[x]-> cal2 
      | h::j::t -> min cal2 (h+j)
    in
    cal2;;


malo [-42; -12; -8; -1; -1; 5; 15; 60];;
malo [-45; -5; 6; 45];;
malo [-59; -45; -5; 1; 30; 47; 50];;