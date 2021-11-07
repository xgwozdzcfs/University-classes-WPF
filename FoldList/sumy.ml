open List;;
let sumy lista =
    rev(fold_left (fun a e -> (e+ hd a)::a) [0] lista);;

assert(sumy [1;2;3;4;5]=[0;1;3;6;10;15]);;
assert(sumy [1;-1;3;4]=[0;1;0;3;7]);;
