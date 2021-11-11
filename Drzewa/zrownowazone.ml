open List;;
type 'a tree =  Node of 'a * 'a tree list;;
let rec fold_tree f (Node (x, l)) = 
  f x (map (fold_tree f) l);;

exception Nie;;

let zrownowazone drzewo =
  let porownaj l =
    match l with
    | [] -> -1 
    | [x] -> x 
    | h::t -> fold_left (fun a e -> if e = a then a else raise Nie) h t 
  in 
  try 
    let _ = fold_tree (fun _ l -> porownaj l + 1) drzewo in true
  with
    Nie -> false;;


let drzewko = Node(1, [(Node(2, [])); Node(3, [Node(4, [])]); Node(5, [Node(6, [Node(7,[])])])]);; 
zrownowazone drzewko;; 
let drzewko2 = Node(1, []);;
zrownowazone drzewko2;;
let drzewko3 = Node(1, [Node (2, [Node(3, []); Node(4, [])]); Node (5, [Node(6, []); Node(7, [])])]);;
zrownowazone drzewko3;;
