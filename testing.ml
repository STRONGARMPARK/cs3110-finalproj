open Graphs
open Evolution1d;;

module Test = Graphs.Make (FreeParticleEvolutionEulers1D)

let w = [{Complex.re=1.0;im=3.0};{Complex.re=2.0;im=1.0};
          {Complex.re=(-0.3);im=1.2};{Complex.re=3.0;im=3.0};
          {Complex.re=1.0;im=3.0};{Complex.re=1.0;im=3.0};]

let rec main () =
  print_string "w -> wave, p -> prob\n";
  match read_line () with 
  | "w" -> Test.graph_wave (-4., 4.) w "periodic"
  | "p" -> Test.graph_prob (-4., 4.) w "periodic"
  | _ -> print_endline "no."

let () = main ()