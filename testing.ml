open Graphs
open Graphs2d
open Evolution;;

module Test = Graphs.Make (FreeParticleEvolutionEulers1D)
module Test2d = Graphs2d.Make (FreeParticleEvolutionSpectral2D)

let w = [{Complex.re=1.0;im=3.0};{Complex.re=2.0;im=1.0};
          {Complex.re=(-0.3);im=1.2};{Complex.re=3.0;im=3.0};
          {Complex.re=1.0;im=3.0};{Complex.re=1.0;im=3.0};]

let w2d = [
    [{Complex.re=5.0;im=10.0};{Complex.re=4.0;im=12.0}];
    [{Complex.re=5.0;im=10.0};{Complex.re=8.0;im=3.0}];
]
let main () =
  print_string "1 -> 1D, 2 -> 2D\n> ";
  match read_line () with 
  | "1" -> Test.graph_prob (-4., 4.) w Periodic
  | "2" -> Test2d.graph_prob ((-2., 2.),(-2.,2.)) w2d Periodic
  | _ -> raise (Invalid_argument "no.")

let () = main ()