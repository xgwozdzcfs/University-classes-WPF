open List;;
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;; 
let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node(l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;
exception Nie;;
let avl_shaped tree=
  try
    let _ =fold_tree (fun _ l r -> if abs(l-r)<=1 then max l r + 1 else raise Nie) 0 tree in true
  with
    Nie -> false;; 

avl_shaped(Node(Leaf, 1, Leaf));;
avl_shaped (Node(Leaf, 1,Node(Leaf, 1, Node(Leaf, 1, Leaf))));;