open List;;
let niemalejacy arr =
  let n = Array.length arr in
  if n = 1 then true 
  else if arr.(0)>=0 then true
  else if arr.(n-1) < 0 then false
  else     
    let l = ref 0 and r = ref (n-1) in
    while l < r do 
      let mid = (!l + !r)/2 in
      if arr.(mid) < 0
      then   
        l:= mid+1
      else r:=mid 
    done;
    if arr.(!l)<abs(arr.(0)) then false
    else if arr.(0) = arr.(!l-1) then true
    else false;;

niemalejacy [|2;3;4;5|];;
niemalejacy [|-1; -1; 2; 4; 5; 6|];;
niemalejacy [|-3; -3; 2; 4; 5; 6|];;
niemalejacy [|-2; -1; 2; 4; 5; 6|];;
