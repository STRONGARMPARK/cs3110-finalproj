open Graphs;;
open Evolution;;

module GrapherFPS = Graphs.Make (FreeParticleEvolutionSpectral1D)
module GrapherFPE = Graphs.Make (FreeParticleEvolutionEulers1D)
module GrapherHOE = Graphs.Make (HarmonicOscillatorEvolutionEulers1D)

let rec print_thank_you x = 
  print_endline "Thank you for using our application!";

and print_initial_condition_helper_2d lst = 
  match lst with 
  | x :: xs -> print_initial_condition_helper (List.rev x) 0 ""
  | [] -> failwith "not possible"

and print_initial_condition_helper lst number acc = 
  match number with 
  | 3 -> acc ^ "..." 
  | _ -> 
    match lst with 
    | [] -> acc
    | x :: xs -> 
      let real = string_of_float x.Complex.re in 
      let imaginary = string_of_float x.Complex.im in 
      print_initial_condition_helper xs (number + 1) (acc ^ "(" ^ real ^ "+ i" ^ imaginary ^ ")" ^ ", ")

and print_user_preference_2d dimension solver domain initial_condition boundary_condition print_boundary print_neumann = 
  begin let _ = print_endline "Dimensions: 2" in 
        let _ = match solver with 
        | "fps" -> 
          print_endline "Solver: Free Particle Spectral"; 
        | "fpe" -> 
          print_endline "Solver: Free Particle Eulers";
        | "hoe" -> 
          print_endline "Solver: Harmonic Oscillator";
        | _ -> (); in 
        let _ = match domain with 
        | ((f,s), (t,p)) -> 
          if f = 0.0 && s = 0.0 && t = 0.0 && p = 0.0 then () else begin 
          print_string "Domain (("; print_float f; print_string ", "; print_float s; print_string ")"; print_string ", ("; print_float t; print_string ", "; print_float p; print_endline "))"; end in 
        let _ = match initial_condition with
        | [] -> ();
        | _ -> 
          print_string "Initial Condition First Column: "; print_endline (print_initial_condition_helper_2d initial_condition); in 
        let _ = match boundary_condition with 
        | Periodic -> 
          if not print_boundary then () else 
          print_endline "Boundary condition: Periodic";
        | Neumann (x,y) -> 
          if not print_neumann then () else begin 
          print_string "Boundary condition: Neumann ";
          print_string "("; print_float x.Complex.re; print_string "+ i"; print_float x.Complex.im; print_string ")";
          print_string ", ("; print_float y.Complex.re; print_string "+ i"; print_float y.Complex.im; print_string ")"   end 
        | Dirichlet -> 
          if not print_boundary then () else
          print_endline "Boundary condition: Dirichlet"; in 
        (); end 

and print_user_preference dimension solver domain initial_condition boundary_condition print_boundary print_neumann =
  begin 
    let _ = 
      match dimension with 
      | 1 -> print_endline "Dimensions: 1"; 
      | 2 -> print_endline "Dimensions: 2";
      | _ -> failwith "not possible" in 
    let _ = match solver with 
    | "fps" -> 
      print_endline "Solver: Free Particle Spectral"; 
    | "fpe" -> 
      print_endline "Solver: Free Particle Eulers";
    | "hoe" -> 
      print_endline "Solver: Harmonic Oscillator";
    | _ -> (); in 
    let _ = match domain with 
    | (x,y) -> 
      if x = 0.0 && y = 0.0 then () else begin 
      print_string "Domain ("; print_float x; print_string ", "; print_float y; print_endline ")"; end in 
    let _ = match initial_condition with
    | [] -> ();
    | _ -> 
      print_string "Initial Condition: "; print_endline (print_initial_condition_helper (List.rev initial_condition) 0 ""); in 
    let _ = match boundary_condition with 
    | Periodic -> 
      if not print_boundary then () else 
      print_endline "Boundary condition: Periodic";
    | Neumann (x,y) -> 
      if not print_neumann then () else begin 
      print_string "Boundary condition: Neumann ";
      print_string "("; print_float x.Complex.re; print_string "+ i"; print_float x.Complex.im; print_string ")";
      print_string ", ("; print_float y.Complex.re; print_string "+ i"; print_float y.Complex.im; print_string ")"   end 
    | Dirichlet -> 
      if not print_boundary then () else
      print_endline "Boundary condition: Dirichlet"; in 
    ();
  end 


and wave_or_prob_1d dimension solver domain initial_condition boundary_condition = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference dimension solver domain initial_condition boundary_condition true true;
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
    | "q" -> print_thank_you 1; Stdlib.exit 0;
    | "b" -> boundary_conditions_one_dimension dimension solver domain initial_condition
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

and neumann_helper dimension solver domain initial_condition = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference dimension solver domain initial_condition Periodic false false;
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
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> boundary_conditions_one_dimension dimension solver domain initial_condition
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
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> neumann_helper dimension solver domain initial_condition
      | string_verse -> 
        let clean_verse = String.trim string_verse in 
        let list_verse_string = String.split_on_char(' ') clean_verse in 
        let list_verse = try List.map float_of_string list_verse_string with 
      | Failure x -> [1.232323] in 
        let length = List.length list_verse in 
        if length mod 2 = 1 || length < 2 || length > 2 then 
          begin print_second := true end 
      else match list_verse with 
      | x :: y :: [] -> begin neumann_second := {Complex.re = x; im = y}; finished_second := true end
      | _ -> failwith "not possible" 
  done; 
  wave_or_prob_1d dimension solver domain initial_condition (Neumann (!neumann_first, !neumann_second))

and boundary_conditions_one_dimension dimension solver domain initial_condition = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference dimension solver domain initial_condition Periodic false false;
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
    | "q" -> print_thank_you 1; Stdlib.exit 0;
    | "b" -> initial_function_one_dimension dimension solver domain
    | _ -> print := true 
  done;
  match !boundary_condition with 
  | Periodic -> wave_or_prob_1d dimension solver domain initial_condition Periodic
  | Dirichlet -> wave_or_prob_1d dimension solver domain initial_condition Dirichlet 
  | Neumann x -> neumann_helper dimension solver domain initial_condition;
  
  wave_or_prob_1d dimension solver domain initial_condition !boundary_condition 

and to_complex_list list acc = 
  match list with
  | [] -> acc 
  | x :: y :: xs -> to_complex_list xs ({Complex.re = x; im = y} :: acc)
  | _ -> failwith "not possible"

and initial_function_one_dimension dimension solver domain = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference dimension solver domain [] Periodic false false;
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
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> domain_one_dimension dimension solver
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
  | "fps" -> wave_or_prob_1d dimension solver domain !initial_condition Periodic
  | _ -> boundary_conditions_one_dimension dimension solver domain !initial_condition

and initial_function_two_dimension dimension solver domain = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference_2d dimension solver domain [] Periodic false false; 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nBecause we are working in 2 dimensions, the initial condition has to be 2 dimensional as well. Therefore, the first set of numbers you will enter will be the first column of the matrix\n";
  print_endline "\nFor example, if you typed, '1 2 3 4 5 6 7 8' then the first column would be (going down) (1 + 2i), (3 + 4i), (5 + 6i), (7 + 8i), keep in mind that each of the columns has to have the same number of terms.";
  print_endline "After you have typed in your last column that you want and it has the right amount of terms and then type 'd' and press enter and you will move on to the next step!";
  print_endline "Also realize that you have to have at least 4 columns and 4 rows.";
  print_endline "\n";
  print_string "> ";
  let print_first = ref false in 
  let print_second = ref false in 
  let finished_first = ref false in 
  let finished_second = ref false in 
  let required_num_in_columns = ref 0 in 
  let num_columns = ref 0 in 
  let initial_condition = ref [] in 
  while not !finished_first do 
    if !print_first then begin 
    ANSITerminal.print_string [ ANSITerminal.red ]
    "\nPlease enter in a valid initial condition. As a reminder you need at least 4 complex numbers (so 8 numbers)";
    print_endline "\n";
    print_string "> "; end else (); 
    match read_line () with 
    | "q" -> print_thank_you 1; Stdlib.exit 0; 
    | "b" -> domain_two_dimension dimension solver 
    | string_verse -> 
      let clean_verse = String.trim string_verse in 
      let list_verse_string = String.split_on_char(' ') clean_verse in 
      let list_verse = try List.map float_of_string list_verse_string with 
      | Failure x -> [1.2232323] in 
      let length = List.length list_verse in 
      if length mod 2 = 1 then begin print_first := true; end 
      else if length < 8 then begin print_first := true; end 
      else let complex_verse = to_complex_list list_verse [] in 
        begin initial_condition := complex_verse :: !initial_condition; finished_first := true; required_num_in_columns := length; num_columns := 1; end 
    done; 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\nPlease input the next column now, remember that it has to have the same number of complex terms as your first column! When you are done press enter, type in 'd' and press enter again!";
  print_endline "\n";
  print_string "> ";
  while not !finished_second || !num_columns < 4 do 
    if !print_second then begin 
    let message = "\nPlease enter a valid column, remember that you need to have the same number of terms in each column, according to your first column you need to input " ^ (string_of_int !required_num_in_columns) ^ " terms or in other words " ^ (string_of_int (!required_num_in_columns / 2)) ^ " more complex numbers" in 
    ANSITerminal.print_string [ ANSITerminal.red ] message; 
    print_endline "\n";
    print_string "> " end else (); 
    match read_line () with 
    | "q" -> print_thank_you 1; Stdlib.exit 0;
    | "b" -> initial_function_two_dimension dimension solver domain
    | "d" -> begin 
      if !num_columns < 4 then begin 
        print_endline "\nYou need to input more columns!";
        print_second := false; 
        print_endline "\n";
        print_string "> ";
      end 
      else 
        finished_second := true; 
      end 
    | string_verse -> 
      let clean_verse = String.trim string_verse in 
      let list_verse_string = String.split_on_char (' ') clean_verse in 
      let list_verse = try List.map float_of_string list_verse_string with 
      | Failure x -> [1.2232323] in 
      let length = List.length list_verse in 
      if length <> !required_num_in_columns then begin print_second := true end 
      else let complex_verse = to_complex_list list_verse [] in 
      begin initial_condition := complex_verse :: !initial_condition; num_columns := !num_columns + 1; print_second := false; 
      ANSITerminal.print_string [ ANSITerminal.cyan ]
      "\nPlease input the next column now, remember that it has to have the same number of complex terms as your first column! When you are done press enter, type in 'd' and press enter again!";
      print_endline "\n";
      print_string "> ";end 
  done; 
  print_user_preference_2d dimension solver domain !initial_condition Periodic false false; 

and domain_one_dimension dimension solver =
  print_endline "\n\n\n\n\n\n";
  print_user_preference dimension solver (0.0, 0.0) [] Periodic false false;
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
      let user_input_first = read_line () in 
      match user_input_first with 
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> solver_helper dimension
      | x -> begin 
      let user_input = try float_of_string x with 
      | Failure x -> 968374657.0 
      in 
      match user_input with 
      | 968374657.0 -> print_first := true
      | x -> 
        if solver = "hoe" && x < 0.0 then 
        begin domain_first := x; finished_first := true; end
        else if solver = "hoe" && x >= 0.0 then begin print_first := true; end 
        else begin domain_first := x; finished_first := true; end end 
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
      let user_input_first = read_line () in 
      match user_input_first with 
      | "q" -> print_thank_you 1; Stdlib.exit 0; 
      | "b" -> domain_one_dimension dimension solver 
      | x -> begin 
      let user_input = try float_of_string x with 
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
        else print_second := true end 
  done; 
  initial_function_one_dimension dimension solver (!domain_first, !domain_second)

and domain_two_dimension dimension solver = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference_2d dimension solver ((0.0, 0.0),(0.0, 0.0)) [] Periodic false false;
  let print_first = ref false in
  let print_second = ref false in 
  let finished_first = ref false in 
  let finished_second = ref false in 
  let domain_first = ref 0.0 in 
  let domain_second = ref 0.0 in 
  let print_third = ref false in
  let print_fourth = ref false in 
  let finished_third = ref false in 
  let finished_fourth = ref false in 
  let domain_third = ref 0.0 in 
  let domain_fourth = ref 0.0 in 
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
      let user_input_first = read_line () in 
      match user_input_first with 
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> solver_helper dimension
      | x -> begin 
      let user_input = try float_of_string x with 
      | Failure x -> 968374657.0 
      in 
      match user_input with 
      | 968374657.0 -> print_first := true
      | x -> 
        if solver = "hoe" && x < 0.0 then 
        begin domain_first := x; finished_first := true; end
        else if solver = "hoe" && x >= 0.0 then begin print_first := true; end 
        else begin domain_first := x; finished_first := true; end end 
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
      let user_input_first = read_line () in 
      match user_input_first with 
      | "q" -> print_thank_you 1; Stdlib.exit 0; 
      | "b" -> domain_two_dimension dimension solver 
      | x -> begin 
      let user_input = try float_of_string x with 
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
        else print_second := true end 
  done; 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input the left bound of the second part of the domain that you want. (Just has to be a number)";
  ANSITerminal.print_string [ ANSITerminal.green ]
  "\n*note: if you are solving the harmonic oscillator your domain has to be symmetric about 0.0.";
  print_endline "\n";
  print_string "> ";
  while not !finished_third do 
    if !print_third then begin
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid left bound.";
      ANSITerminal.print_string [ ANSITerminal.green ]
      "\n*note: if you are solving the harmonic oscillator your domain has to be symmetric about 0.0.\n";
      print_endline "";
      print_string "> "; end else ();
      let user_input_first = read_line () in 
      match user_input_first with 
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> solver_helper dimension
      | x -> begin 
      let user_input = try float_of_string x with 
      | Failure x -> 968374657.0 
      in 
      match user_input with 
      | 968374657.0 -> print_third := true
      | x -> 
        if solver = "hoe" && x < 0.0 then 
        begin domain_third := x; finished_third := true; end
        else if solver = "hoe" && x >= 0.0 then begin print_third := true; end 
        else begin domain_third := x; finished_third := true; end end 
    done; 
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\nNow please input a right bound of the second part of the domain. It has to be greater than your left bound.";
  print_endline "\n";
  print_string "> ";
  while not !finished_fourth do 
    if !print_fourth then begin 
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nPlease input a valid right bound. It has to be greater than your left bound.";
      print_endline "\n";
      print_string "> "; end else ();
      let user_input_first = read_line () in 
      match user_input_first with 
      | "q" -> print_thank_you 1; Stdlib.exit 0; 
      | "b" -> domain_two_dimension dimension solver 
      | x -> begin 
      let user_input = try float_of_string x with 
      | Failure x -> 968374657.0 
      in 
      match user_input with 
      | 968374657.0 -> print_fourth := true
      | x -> 
        if x > !domain_third then 
          if solver = "hoe" then 
            if x = -1.0 *. !domain_third then 
              begin domain_fourth := x; finished_fourth := true; end
        else begin print_fourth := true end 
        else begin domain_fourth := x; finished_fourth := true; end
        else print_fourth := true end 
  done; 
  initial_function_two_dimension dimension solver ((!domain_first, !domain_second),(!domain_third, !domain_fourth))


and solver_helper dimension = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference dimension "no" (0.0, 0.0) [] Periodic false false;
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
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> dimension_starter 1
      | _ -> print := true;
  done;
  match dimension with 
  | 1 -> domain_one_dimension dimension !solver
  | 2 -> domain_two_dimension dimension !solver
  | _ -> failwith "not possible"
  

and dimension_starter x = 
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
      | "q" -> print_thank_you 1; Stdlib.exit 0;
      | "b" -> main ();
      | _ -> print := true;
  done; 
  match !dimension with 
  | 1 -> solver_helper 1
  | 2 -> solver_helper 2
  | _ -> failwith ""


and main () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\nWelcome to the Schrödinger equation solver!\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
  "\nYou can type in 'q' to quit and 'b' to go back to the previous option selector!\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\nPress enter to begin.\n";
  print_endline "";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | x -> dimension_starter 1

let () = main ()