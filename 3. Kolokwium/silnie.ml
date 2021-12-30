open List;;
let silnie (liczba1:int) = 
  let liczba = ref liczba1
  and l = ref [(1,1)] 
  and ans = ref [] in
  let pierwszy = ref (1,1)
  and numer = ref 1 in
  while fst !pierwszy < !liczba do
    numer := !numer + 1;
    pierwszy := ((fst !pierwszy) * (!numer), !numer);
    l := (!pierwszy)::!l;
  done;
  while !liczba > 0 do
    if (fst !pierwszy) <= !liczba then
      begin 
        liczba := !liczba - (fst !pierwszy);
        ans := (snd !pierwszy) :: !ans;
      end
    else
      begin
        l := tl !l;
        pierwszy := hd !l;
      end
  done;
  !ans;;
silnie 42;;