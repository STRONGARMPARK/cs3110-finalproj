
open Graphics
;;
let _ = open_graph " 300x300"

let point x y =
  let _ = set_color black in 
  fill_circle x y 1

let l = [(100,100);(101,101);(102,102);(103,103);(104,104);(105,105);(106,106);]

let _ = List.map (fun p -> point (fst p) (snd p)) l

let _ = wait_next_event [ Key_pressed ]
