open Graphs;;

let rec main () =
  match read_line () with 
  | "1" -> Grapher.graph 1
  | _ -> print_endline "no."

let () = main ()