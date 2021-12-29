open Array;;
let unikat tablica k = 
  let dp = Array.make (k+1) 0 in
  for i = 0 to Array.length tablica - 1 do
    for j = k downto 1 do 
      if dp.(j) > 0 then 
        if j + tablica.(i) <= k then 
          dp.(j+tablica.(i)) <- dp.(j) + dp.(j+tablica.(i));
    done;
    begin
      dp.(tablica.(i)) <- 1 + dp.(tablica.(i));
    end
  done;
  let result = ref 0 in
  for i = 1 to k do
    if dp.(i) = 1 then
      result := i;
  done;
  !result ;;


unikat [|2; 4; 5; 6; 7; 8|] 8;;