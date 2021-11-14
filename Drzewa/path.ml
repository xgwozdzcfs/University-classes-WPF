open List;;
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;; 
let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node(l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;


let path tree=
  let tree =snd( fold_tree (fun x (d1,l) (d2, r) -> if d1 > d2 then (d1+1, x::l) else (d2+1, x::r)) (0, []) tree) in
  tree;;

let drzewko1 = Node(Node(Leaf,1,Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Node(Leaf, 5, Node(Leaf, 6,Leaf))));;
path drzewko1;;