open Graphs
open Graphs2d
open Evolution;;

module Test = Graphs.Make (FreeParticleEvolutionEulers1D)
module Test2d = Graphs2d.Make (FreeParticleEvolutionSpectral2D)

let w = [{Complex.re=1.0;im=3.0};{Complex.re=2.0;im=1.0};
          {Complex.re=(-0.3);im=1.2};{Complex.re=3.0;im=3.0};
          {Complex.re=1.0;im=3.0};{Complex.re=1.0;im=3.0};]

let w2d = [
    [{Complex.re=1.0;im=3.0};{Complex.re=2.0;im=1.0};
     {Complex.re=(-0.3);im=1.2};{Complex.re=3.0;im=3.0}];
    [{Complex.re=1.0;im=3.0};{Complex.re=1.0;im=3.0};
     {Complex.re=(-0.3);im=1.2};{Complex.re=3.0;im=3.0}]
  ]

let dim () =
  print_string "1 -> 1D, 2 -> 2D\n> ";
  match read_line () with 
  | "1" -> true
  | "2" -> false
  | _ -> raise (Invalid_argument "no.")

let main () =
  let is1d = dim () in
  print_string "w -> wave, p -> prob\n> ";
  match read_line () with 
  | "w" -> (if is1d then Test.graph_wave (-4., 4.) w Periodic
            else Test2d.graph_wave ((-4., 4.),(-4.,4.)) w2d [Periodic;Periodic])
  | "p" -> (if is1d then Test.graph_prob (-4., 4.) w Periodic
            else Test2d.graph_prob ((-4., 4.),(-4.,4.)) w2d [Periodic;Periodic])
  | _ -> raise (Invalid_argument "no.")

let () = main ()