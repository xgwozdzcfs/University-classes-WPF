open List;;
type 'a tree = Node of 'a * 'a tree *  'a tree | Leaf;; 
let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node(x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

let skosokosc tree = 
  let f x l r =
    match l, r with
    | Leaf, Leaf -> Node( (x,1), Leaf,  Leaf)
    | Node( (w1, v1), _,  _), Leaf -> Node( (x, 2*v1), l,  r)
    | Leaf, Node((w1, v1), _,  _) -> Node( (x, 2*v1), r,  l)
    | Node( (w1, v1) , _,  _), Node( (w2, v2), _,  _) -> 
        if max (2*v1) (2*v2+1) <= max (2*v1+1) (2*v2)
        then  Node( (x ,max (2*v1) (2*v2+1)), l,  r) 
        else Node( (x, max (2*v1+1) (2*v2)), r,  l)
  in 
  let pomtree =
    fold_tree (f) Leaf tree in 
  let f2 (w, v) l r =
    Node (w, l, r) in
  fold_tree f2 Leaf pomtree;;


let tree1 = 
  Node(5, 
       Node(4, Leaf, Node(2, Leaf, Leaf)),
       Node(6, 
            Node(1, Leaf, Leaf),
            Node(3, Leaf, Leaf)
           )
      )
;;
skosokosc tree1;;
  
  