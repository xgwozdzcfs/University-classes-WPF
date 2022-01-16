
let rozklady2 n = 
  let rec fib f1 f2 ak =
    if f1 > n then tl ak
    else if f1 = n then ak
    else fib (f1+f2) f1 ((f1+f2)::ak)
  in 
  let tabf = of_list (rev (fib 2 1 [2; 1])) in 
  let m = Array.length tabf in
  let dp = make_matrix (n+1) (m + 1) (0) in
  dp.(0).(0) <- 1; 
  for j = 0 to m - 1 do
    for i = n downto 0 do
      dp.(i).(j+1) <- dp.(i).(j);
      if i + tabf.(j) <= n then
        dp.(i+tabf.(j)).(j+1) <- dp.(i+tabf.(j)).(j+1) + dp.(i).(j);
    done;
  done;
  dp.(n).(m);;