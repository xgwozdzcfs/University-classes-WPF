open Array;;
let zwroc tab =
  let n = length tab in
  let odw = make n false and wyn = ref 0 in
  for i = 0 to n-1 do
    if odw.(tab.(i)-1)=false
    then odw.(tab.(i)-1) <- true
    else wyn := tab.(i)
  done; !wyn;;
zwroc [|1;2;3;2|];;
zwroc [|1;2;3;5;4;3;6;7;8|];;
