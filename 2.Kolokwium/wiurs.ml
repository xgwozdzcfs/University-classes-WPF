open List;;
let wirus lista =
  let n = length lista in
  let tab2 = Array.make n (0,0) in
  let list2 = ref lista in
  let m = ref 0 in
  for i=0 to n-1 do
    m := max !m (snd (hd !list2));
    tab2.(i) <- (hd !list2);
    list2 := tl !list2;
  done; 
  let wsiedli = Array.make (!m+2) 0 in
  let byli = Array.make (!m+2) 0 in
  for i = 0 to n-1 do
    let (p,k) = tab2.(i) in
    wsiedli.(p) <- wsiedli.(p)+1;
    byli.(p) <- byli.(p)+1;
    byli.(k) <- byli.(k)-1;
  done;
  for i = 1 to !m do
    wsiedli.(i) <- wsiedli.(i)+wsiedli.(i-1);
    byli.(i) <- byli.(i)+byli.(i-1); 
  done;
  let wynik = ref [] in
  for i = 0 to n-1 do
    let (p,k) =tab2.(i) in
    let b = wsiedli.(k-1)-wsiedli.(p) in
    wynik := (byli.(p)+b-1)::!wynik;
  done;
  (* byli;;*)
  rev !wynik;;
wirus  [(2,5);(4,7);(1,8);(3,4);(4,5);(8,9);(6,7)];;