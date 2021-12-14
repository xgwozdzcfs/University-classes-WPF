open List;;
let rozne arr = 
  let n = Array.length arr in
  if n = 1 then true 
  else if arr.(n-1)-arr.(n-2) < 0 || arr.(1) - arr.(0)>0 then true
  else 
    let gdzie = ref (-1) in
    let wynik = ref true in
    for i = n-2 downto 0 do
      if arr.(i+1)-arr.(i)>=0 then 
        gdzie := i;
      if arr.(i+1)=arr.(i) then 
        wynik:= false;
    done;
    if !wynik = false then false
    else
      let l= ref (!gdzie-1) and r = ref (!gdzie + 1) in 
      while !l>=0 && !r<=n-1 do
        if arr.(!l) = arr.(!r) then
          begin
            wynik := false;
            l := -1;
          end
        else if arr.(!l) > arr.(!r) 
        then 
          begin r:= !r+1; end
        else
          l:= !l -1;
      done; 
      !wynik
;;
rozne [|28;20;15;12;11;13;14;15;16;23|];;
rozne [|28;20;15;12;11;13;14;16;23|];;