open List;; 
type 'a tree =  Node of 'a * 'a tree list;;
 
let rec fold_tree f (Node (x, l)) = 
  f x (map (fold_tree f) l);;

let liscie t = 
  let rec pom (Node (x, l)) ak = 
    match ak with
    | [] -> if l = [] then [1] else 0::(fold_left (fun a e -> pom e a) [] l)
    | p::q -> if l = [] then (p+1)::q else p::(fold_left (fun a e -> pom e a) q l) 
  in pom t [0];;

let drzewko = Node(1, [Node(2, []); Node(3, [Node(4, [])]); Node(4, [])]);; 
liscie drzewko;;
let drzewko = Node(1, []);;
liscie drzewko;;
let drzewko = Node(1, [Node(2, [])]);;
liscie drzewko;;
let drzewko = Node(1, [Node(2, [Node(4, [])]); Node(3, [])]);;
liscie drzewko;;