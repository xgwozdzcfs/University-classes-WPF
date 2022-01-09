open Array;;
let test poj tab =
  let ile = ref 0 in 
  for i = 0 to length tab - 1 do
    ile := max 0 (!ile + tab.(i) - poj);
  done;
  !ile <= 0;;

let autobusy2 pas kontr =
  let n = Array.length pas in
  let m = Array.length kontr in
  let ans = ref true in
  for i = 0 to n-1 do
    if i >kontr.(m-1) && pas.(i) > 0 then
      ans := false;
    if i > 0 then
      pas.(i) <- pas.(i) + pas.(i-1);
  done;
  if !ans = false then -1 
  else
    let poc = ref 0 
    and kon = ref pas.(n-1) in
    let pas2 = make m 0 in
    for i = m-1 downto 0 do
      pas2.(i) <- pas.(kontr.(i));
      if i < m-1 then
        pas2.(i+1) <- pas2.(i+1) - pas2.(i);
    done; 
    while !poc < !kon do
      let mid = (!poc + !kon) / 2 in 
      if test mid pas2 = true then
        begin
          kon:= mid;
        end
      else
        begin
          poc := mid+1;
        end
    done;
    !poc;;
