open List;;
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;; 
let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node(l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;

let id = fun x -> x;; 
let pre_order t= 
  let f x dl dr= fun acc ->  (dl (dr (x::acc)))
  in fold_tree (f) (id) t [];;

pre_order (Node(Leaf, 1, Leaf));;
pre_order (Node(Node(Node(Leaf, 4, Leaf), 1 , Node(Leaf, 5, Leaf)), 2, Node(Node(Leaf, 6, Leaf), 3, Node(Leaf, 7, Leaf))));;
