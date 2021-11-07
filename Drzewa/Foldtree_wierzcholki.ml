open List;;
type 'a tree =  Node of 'a * 'a tree list;;
let rec fold_tree f (Node (x, l)) = 
  f x (map (fold_tree f) l);;

let liczbawezlow drzewo =
  let suma l = fold_left (+) 0 l
  in
  fold_tree (fun _ l ->suma l + 1) drzewo;;

let drzewko = Node(1, [(Node(2, [])); Node(3, [Node(4, [])]); Node(5, [Node(6, [Node(7,[])])])]);;

liczbawezlow drzewko;; 
