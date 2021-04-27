open Graphs;;
open Evolution;;

module GrapherFPS = Graphs.Make (FreeParticleEvolutionSpectral1D)
module GrapherFPE = Graphs.Make (FreeParticleEvolutionEulers1D)
module GrapherHOE = Graphs.Make (HarmonicOscillatorEvolutionEulers1D)

let rec wave_or_prob solver domain initial_condition boundary_condition = 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nFinally, would you like the wave function or the probability distribution? Again much like before just type 1 or 2 and press enter for the option that you want.";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (1) Wave Function";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (2) Probability Distribution";
  print_endline "\n";
  print_string "> ";
  let print = ref false in 
  let finished = ref false in 
  let wop = ref "" in 
  while not !finished do 
    if !print then begin 
    ANSITerminal.print_string [ ANSITerminal.red ]
    "\nPlease input a valid option (1 or 2)\n"; end else ();
    match read_line () with 
    | "1" -> finished := true; wop := "wave"
    | "2" -> finished := true; wop := "probability"
    | _ -> print := true
  done; 
  match !wop with 
  | "wave" -> begin
    match solver with 
    | "fps" -> GrapherFPS.graph_wave domain initial_condition boundary_condition
    | "fpe" -> GrapherFPE.graph_wave domain initial_condition boundary_condition
    | "hoe" -> GrapherHOE.graph_wave domain initial_condition boundary_condition
    | _ -> failwith "not possible" end
  | "probability" -> begin
    match solver with 
    | "fps" -> GrapherFPS.graph_prob domain initial_condition boundary_condition
    | "fpe" -> GrapherFPE.graph_prob domain initial_condition boundary_condition
    | "hoe" -> GrapherHOE.graph_prob domain initial_condition boundary_condition
    | _ -> failwith "not possible" end
  | _ -> failwith "not possible"


let rec neumann_helper solver domain initial_condition = 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input the derivative of the left endpoint that you would like. Has to be a complex number and it is formatted like before";
  print_endline "\n";
  print_string "> ";
  let print_first = ref false in
  let print_second = ref false in 
  let finished_first = ref false in 
  let finished_second = ref false in 
  let neumann_first = ref Complex.zero in 
  let neumann_second = ref Complex.zero in 
  while not !finished_first do 
    if !print_first then begin
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid complex number (only two numbers separated by spaces)";
      print_endline "\n";
      print_string "> "; end else ();
      match read_line () with 
      | string_verse -> 
        let clean_verse = String.trim string_verse in 
        let list_verse_string = String.split_on_char(' ') clean_verse in 
        let list_verse = try List.map float_of_string list_verse_string with 
      | Failure x -> [1.232323] in 
        let length = List.length list_verse in 
        if length mod 2 = 1 || length < 2 || length > 2 then 
          begin print_first := true end 
      else match list_verse with 
      | x :: y :: [] -> begin neumann_first := {Complex.re = x; im = y}; finished_first := true end
      | _ -> failwith "not possible" 
  done; 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input the derivative of the right endpoint that you would like. Has to be a complex number and it is formatted like before";
  print_endline "\n";
  print_string "> ";
  while not !finished_second do 
    if !print_second then begin
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid complex number (only two numbers separated by spaces)";
      print_endline "\n";
      print_string "> "; end else ();
      match read_line () with 
      | string_verse -> 
        let clean_verse = String.trim string_verse in 
        let list_verse_string = String.split_on_char(' ') clean_verse in 
        let list_verse = List.map float_of_string list_verse_string in 
        let length = List.length list_verse in 
        if length mod 2 = 1 || length < 2 || length > 2 then 
          begin print_second := true end 
      else match list_verse with 
      | x :: y :: [] -> begin neumann_second := {Complex.re = x; im = y}; finished_second := true end
      | _ -> failwith "not possible" 
  done; 
  wave_or_prob solver domain initial_condition (Neumann (!neumann_first, !neumann_second))

let rec boundary_conditions_one_dimension solver domain initial_condition = 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input the boundary condition that you want to have. Much like the solver just type in the number of the option that you want from the list.";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (1) Periodic";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (2) Dirichlet";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (3) Neumann";
  print_endline "\n";
  print_string "> ";
  let print = ref false in 
  let finished = ref false in 
  let boundary_condition = ref Periodic in 
  while not !finished do 
    if !print then begin 
    ANSITerminal.print_string [ ANSITerminal.red ]
    "\nPlease input a valid option (1, 2 or 3)\n"; 
    print_endline "\n";
    print_string "> ";end else ();
    match read_line () with 
    | "1" -> finished := true; boundary_condition := Periodic
    | "2" -> finished := true; boundary_condition := Dirichlet
    | "3" -> finished := true; boundary_condition := Neumann (Complex.zero, Complex.zero)
    | _ -> print := true 
  done;
  match !boundary_condition with 
  | Periodic -> wave_or_prob solver domain initial_condition Periodic
  | Dirichlet -> wave_or_prob solver domain initial_condition Dirichlet 
  | Neumann x -> neumann_helper solver domain initial_condition;
  
  wave_or_prob solver domain initial_condition !boundary_condition 

let rec to_complex_list list acc = 
  match list with
  | [] -> acc 
  | x :: y :: xs -> to_complex_list xs ({Complex.re = x; im = y} :: acc)
  | _ -> failwith "not possible"

let rec initial_function_one_dimension solver domain = 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input your initial function. You need to input at least 4 complex values.";
  ANSITerminal.print_string [ ANSITerminal.green ]
  "\nFormatting works as follows: '1 2 3 4' maps to '1 + 2i', '3 + 4i'";
  print_endline "\n";
  print_string "> ";
  let print = ref false in 
  let finished = ref false in 
  let initial_condition = ref [] in 
  while not !finished do
    if !print then begin 
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease enter in a valid initial condition. As a reminder you need at least 4 complex numbers (so 8 numbers)";
      print_endline "\n";
      print_string "> "; end else ();
      match read_line () with 
      | string_verse -> 
        let clean_verse = String.trim string_verse in 
        let list_verse_string = String.split_on_char(' ') clean_verse in 
        let list_verse = try List.map float_of_string list_verse_string with 
        | Failure x -> [1.2232323] in 
        let length = List.length list_verse in 
        if length mod 2 = 1 then begin print := true; end 
        else if length < 8 then begin print := true; end 
        else let complex_verse = to_complex_list list_verse [] in 
          begin initial_condition := complex_verse; finished := true end 
  done; 
  match solver with 
  | "fps" -> wave_or_prob solver domain !initial_condition Periodic
  | _ -> boundary_conditions_one_dimension solver domain !initial_condition

let rec domain_one_dimension solver =
  let print_first = ref false in
  let print_second = ref false in 
  let finished_first = ref false in 
  let finished_second = ref false in 
  let domain_first = ref 0.0 in 
  let domain_second = ref 0.0 in 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input the left bound of the domain that you want. (Just has to be a number)";
  ANSITerminal.print_string [ ANSITerminal.green ]
  "\n*note: if you are solving the harmonic oscillator your domain has to be symmetric about 0.0.";
  print_endline "\n";
  print_string "> ";
  while not !finished_first do 
    if !print_first then begin
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid left bound.";
      ANSITerminal.print_string [ ANSITerminal.green ]
      "\n*note: if you are solving the harmonic oscillator your domain has to be symmetric about 0.0.\n";
      print_endline "";
      print_string "> "; end else ();
      let user_input = try float_of_string (read_line ()) with 
      | Failure x -> 968374657.0 
      in 
      match user_input with 
      | 968374657.0 -> print_first := true
      | x -> 
        if solver = "hoe" && x < 0.0 then 
        begin domain_first := x; finished_first := true; end
        else if solver = "hoe" && x >= 0.0 then begin print_first := true; end 
        else begin domain_first := x; finished_first := true; end
    done; 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\nNow please input a right bound. It has to be greater than your left bound.";
  print_endline "\n";
  print_string "> ";
  while not !finished_second do 
    if !print_second then begin 
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid right bound. It has to be greater than your left bound.";
      print_endline "\n";
      print_string "> "; end else ();
      let user_input = try float_of_string (read_line ()) with 
      | Failure x -> 968374657.0 
      in 
      match user_input with 
      | 968374657.0 -> print_second := true
      | x -> 
        if x > !domain_first then 
          if solver = "hoe" then 
            if x = -1.0 *. !domain_first then 
              begin domain_second := x; finished_second := true; end
        else begin print_second := true end 
        else begin domain_second := x; finished_second := true; end
        else print_second := true
  done; 
  initial_function_one_dimension solver (!domain_first, !domain_second)


let rec solver_one_dimension x = 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nWhich solver would you like to use (type in 1, 2, or 3 to choose from the options):";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (1) Free Particle Spectral";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (2) Free Particle Eulers";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (3) Harmonic Oscillator Eulers";
  print_endline "\n";
  print_string "> ";
  let print = ref false in 
  let solver = ref "" in 
  let finished = ref false in 
  while not !finished do 
    if !print then begin 
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease select a valid option.";
      ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\n|> (1) Free Particle Spectral";
      ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\n|> (2) Free Particle Eulers";
      ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\n|> (3) Harmonic Oscillator Eulers";
      print_endline "";
      print_string "> "; end else ();
      match read_line () with 
      | "1" -> finished := true; solver := "fps"; 
      | "2" -> finished := true; solver := "fpe"; 
      | "3" -> finished := true; solver := "hoe"; 
      | _ -> print := true;
  done;
  domain_one_dimension !solver

let rec dimension_starter x = 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease type in how many dimensions you would like us to solve in.\n";
  print_endline "";
  print_string "> ";
  let print = ref false in 
  let dimension = ref 0 in 
  let finished = ref false in 
  while not !finished do 
    if !print then begin
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid number of dimensions (either 1 or 2)\n";
      print_endline "";
      print_string "> "; 
    end else (); 
      match read_line () with 
      | "1" -> finished := true; dimension := 1;
      | "2" -> finished := true; dimension := 2;
      | _ -> print := true;
  done; 
  match !dimension with 
  | 1 -> solver_one_dimension 1
  | 2 -> print_endline "TODO implement for 2 dimensions"
  | _ -> failwith ""


let rec main () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\nWelcome to the Schrödinger equation solver!\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\nPress enter to begin.\n";
  print_endline "";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | x -> dimension_starter 1

let () = main ()