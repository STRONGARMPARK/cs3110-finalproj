open Graphs
open Evolution1d;;

module Test = Graphs.Make (FreeParticleEvolutionEulers1D)

let rec main () =
  match read_line () with 
  | "1" -> Test.graph 1;
  | _ -> print_endline "no."

let () = main ()