let autobusy pas kontr =
  let n = Array.length pas in
  let m = Array.length kontr in
  let ans = ref true in
  for i = 0 to n-1 do
    if i > kontr.(m-1) && pas.(i) > 0 then
      ans := false;
    if i > 0 then
      pas.(i) <- pas.(i) + pas.(i-1);
  done; 
  if !ans = false then -1 
  else
    let pas2 = make m 0 in
    for i = m-1 downto 0 do
      pas2.(i) <- pas.(kontr.(i));
      if i < m-1 then
        pas2.(i+1) <- pas2.(i+1) - pas2.(i);
    done; 
    let ans = ref 0 
    and  ile = ref 0 
    in
    for i = m-1 downto 0 do
      ile := !ile + pas2.(i);
      let contr = m - i in
      if !ile - contr * !ans > 0 then
        ans := !ile / contr;
      if !ile - contr * !ans > 0 then
        ans := !ans+1;
    done;
    !ans
;;
