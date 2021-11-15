open List;; 
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;; 
let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node(l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;

let levels t = 
  let f x l r = function
    | [] -> [x]::l (r []) 
    | h::t -> (x::h)::l (r t) in
  fold_tree f (fun x -> x) t [];;
        
let drzewko1 = Node(Leaf, 3, Node(Node(Leaf, 5, Leaf), 4, Node(Leaf, 6, Leaf)));;
levels drzewko1;;
