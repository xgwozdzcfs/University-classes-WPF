type 'a tree = Empty | Node of 'a tree * 'a * 'a tree;;

let rec fold_tree f a t = 
    match t with 
    | Empty -> a 
    | Node(l, v, r) -> f v (fold_tree f a l) (fold_tree f a r);;

let treeheights tree = 
    let maks v l r = 
        match l,r with 
        | Empty, Empty -> Node(Empty, 0, Empty)
        | Node(_,w,_), Empty -> Node(l, w+1, Empty)
        | Empty, Node(_,w,_)-> Node( Empty, w+1, r)
        | Node(_,w1,_), Node(_, w2, _) -> Node(l, max w1 w2 +1, r) in
    fold_tree  maks Empty tree;;

let a = Node(Node(Empty, 1, Empty), 2, Node(Node(Empty, 1, Empty), 2, Empty));;



treeheights a;;