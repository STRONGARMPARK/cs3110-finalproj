open Graphs;;
open Evolution;;

module GrapherFPS1d = Graphs.Make (FreeParticleEvolutionSpectral1D)
module GrapherFPE1d = Graphs.Make (FreeParticleEvolutionEulers1D)
module GrapherHOE = Graphs.Make (HarmonicOscillatorEvolutionEulers1D)

module GrapherFPS2d = Graphs2d.Make (FreeParticleEvolutionSpectral2D)
module GrapherFPE2d = Graphs2d.Make (FreeParticleEvolutionEulers2D)

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
        | Dirichlet -> 
          if not print_boundary then () else
          print_endline "Boundary condition: Dirichlet"; 
        | _ -> failwith "not possible no Neumann in 2d" in 
        (); end 

and print_user_preference_1d dimension solver domain initial_condition boundary_condition print_boundary print_neumann =
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

and final_check_2d dimension solver domain initial_condition boundary_condition = 
  print_endline "\n\n\n\n\n\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nBelow you will find your selected choices, just press enter to continue to see your graph evolve!";
  print_endline "\n";
  print_user_preference_2d dimension solver domain initial_condition boundary_condition true true;
  print_endline "\n";
  print_string "> ";
  match read_line () with 
  | _ -> 
    match solver with 
    | "fps" -> GrapherFPS2d.graph_prob domain initial_condition boundary_condition
    | "fpe" -> GrapherFPE2d.graph_prob domain initial_condition boundary_condition
    | _ -> failwith "not possible" 

and final_check_1d dimension solver domain initial_condition boundary_condition = 
  print_endline "\n\n\n\n\n\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nBelow you will find your selected choices, just press enter to continue to see your graph evolve!";
  print_endline "\n";
  print_user_preference_1d dimension solver domain initial_condition boundary_condition true true;
  print_endline "\n";
  print_string "> ";
  match read_line () with 
  | _ -> 
    match solver with 
    | "fps" -> GrapherFPS1d.graph_prob domain initial_condition boundary_condition
    | "fpe" -> GrapherFPE1d.graph_prob domain initial_condition boundary_condition
    | "hoe" -> GrapherHOE.graph_prob domain initial_condition boundary_condition
    | _ -> failwith "not possible" 
  
(*[neumann_helper dimension solver domain initial_condition] takes in the 
dimension, solver, domain and initial_condition and is used to help with 
the making of the derivatives at each boundary for neumann for one dimension. 
Keep in mind that for two dimensions we only have two boundary conditions. 
As always helpful messages will be printed, and if everything runs 
smoothly, then they will be taken to the final stage where they can review their
options and graph the probability density function. *)
and neumann_helper dimension solver domain initial_condition = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference_1d dimension solver domain initial_condition Periodic false false;
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
  final_check_1d dimension solver domain initial_condition (Neumann (!neumann_first, !neumann_second))

and boundary_conditions_two_dimension dimension solver domain initial_condition =
  print_endline "\n\n\n\n\n\n";
  print_user_preference_2d dimension solver domain initial_condition Periodic false false;
  ANSITerminal.print_string [ ANSITerminal.cyan ]
  "\n\n\nPlease input the boundary condition that you want to have. Much like the solver just type in the number of the option that you want from the list.";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (1) Periodic";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
  "\n|> (2) Dirichlet";
  print_endline "\n";
  print_string "> ";
  let print = ref false in 
  let finished = ref false in 
  let boundary_condition = ref Periodic in 
  while not !finished do 
    if !print then begin 
    ANSITerminal.print_string [ ANSITerminal.red ]
    "\nPlease input a valid option (1 or 2)\n"; 
    print_endline "\n";
    print_string "> ";end else ();
    match read_line () with 
    | "1" -> finished := true; boundary_condition := Periodic
    | "2" -> finished := true; boundary_condition := Dirichlet
    | "q" -> print_thank_you 1; Stdlib.exit 0;
    | "b" -> initial_function_two_dimension dimension solver domain
    | _ -> print := true 
  done;
  match !boundary_condition with 
  | Periodic -> final_check_2d dimension solver domain initial_condition Periodic
  | Dirichlet -> final_check_2d dimension solver domain initial_condition Dirichlet 
  | _ -> failwith "not possible given error checking"


(*[boundary_conditions_one_dimension dimension solver domain initial_condition]
takes in all of the variables that have been listed, and like the solver 
helper, gives the user a list to choose from as to what boundary condition 
they want. A tricky part about this is that if they chose the Neumann boundary 
condition, then they would actulaly have to be taken to a different helper as 
Neumann requires a bit of an actual initial_condition to represent the 
derivatives at the boundaries.*)
and boundary_conditions_one_dimension dimension solver domain initial_condition = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference_1d dimension solver domain initial_condition Periodic false false;
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
  | Periodic -> final_check_1d dimension solver domain initial_condition Periodic
  | Dirichlet -> final_check_1d dimension solver domain initial_condition Dirichlet 
  | Neumann x -> neumann_helper dimension solver domain initial_condition;
  

and to_complex_list list acc = 
  match list with
  | [] -> acc 
  | x :: y :: xs -> to_complex_list xs ({Complex.re = x; im = y} :: acc)
  | _ -> failwith "not possible"

(*[initial_function_one_dimension dimension solver domain] does exactly 
what one would expect it to gien the name. It takes in a dimension, solver,
and domain, and gives users instructions to inptu their initial_function 
or a series of complex numbers. It will also give helpful hints if the 
user ever does something that is against what our back-end can handle. 
From here, if everything goes succesfully, this will be passed onto the 
boundary condition helper for one dimension with the given dimension solver
and domain, and now also with the initial function which is just a 
list of complex numbers. Another thing we had to watch out for is that
if they gave they chose the Free Particle Euler's solver, then 
we would have to take them straight to the grapher with Periodic as their 
boundary_condition.*)
and initial_function_one_dimension dimension solver domain = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference_1d dimension solver domain [] Periodic false false;
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
  | "fps" -> final_check_1d dimension solver domain !initial_condition Periodic
  | _ -> boundary_conditions_one_dimension dimension solver domain !initial_condition


(*[initial_function_two_dimension dimension solver domain] does exactly what 
it sounds like it does, which is it takes in the dimension, solver, and domain,
which as preconditions should be valid as error handling should be done by 
previous functions. The working of the initial_function for two dimensions
was a little bit trickier. This was because we had to take in a matrix of 
complex numbers with quite a few restrictions as well. The restrictions being 
that first of all they had to be valid complex numbers, second of all we
needed for the number of columns to be greater than or equal to 4 and same for 
the rows, third of all after the first column was inputed, we needed every 
other column to obviously have the same number of elements, and then error
handling messages to the user for all of these possibilities as well. If the 
user does not input a correct list of complex numbers 
(i.e. something that isn't even or has less than the required number for a 
column), the interface will provide helpful hints to the reader. It will 
even tell the user how many elements they hav eot type in for subsequent 
columns, based on their input for the first column. If all is succesful, then 
this will be passed on to boundary_condition helper for two dimensions.*)
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
  boundary_conditions_two_dimension dimension solver domain !initial_condition;

(*[domain_one_dimension dimension solver] does exactly what the two 
dimensional counterpart does, but instead with the idea that it is solving in 
one dimension. Again there are quite a few things that we have to
account for here. Number one is the fact that again the left bound has to be
less than the right bound. Another thing is that if they chose
Harmonic Oscillator as their solver then it has to be symmetric about the 
origin. However, this actually entails checking 2 thing. At first checking
that the first boudn is nothing less than or equal to 0, and then making
sure that the second one is the positive coutnerpart. Along with this
we also have checking for any faulty inputs such as letters or white spaces
etc. *)
and domain_one_dimension dimension solver =
  print_endline "\n\n\n\n\n\n";
  print_user_preference_1d dimension solver (0.0, 0.0) [] Periodic false false;
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


(*[domain_two_dimension dimension solver] takes in a dimension that you want to 
solve in, and the solver that they chose from previous functions. As alwyas 
it prints out the current prefences the user has chosen. There are quite a few
accounting things that we have to take care of for each of the boundaries. 
Because we are in two dimensions, we actually have ot input 4 numbers instead of 
2. One of the things that the interface had to take care of is that the left
bound had to be less than the right bound. Another restriction is that if
the user chose the Harmonic Oscillator as their solver, their domain has to be 
symmetric about the origin. Which lead to much more error handling. If all is 
succesful, then the users preferred domain will be passed on to the 
two dimensional initial_function handler. Also note that different 
error messages had to be made and different instructions had to be made
dependent on whether they were working on their first domain or second domain.*)
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


(*[solver_helper dimension] takes in the dimension that the user chose
from the previous function and allows the user to choose from a list of solvers
to continue. Also notice that in the beginning, the user's current selected 
preferences will show up on the screen. If they enter a valid option, then 
dependent on the dimension that they wanted to solve in, they will either be 
taken to the domain_one_dimension or domain_two_dimension helper to pick 
out their domains. There is also of course error handling to help guide the 
user through the process.*)
and solver_helper dimension = 
  print_endline "\n\n\n\n\n\n";
  print_user_preference_1d dimension "no" (0.0, 0.0) [] Periodic false false;
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
  
(*[dimension_starter x] represents the initial starting configuration with 
no dimension configured yet. If the user types in 'q', the application will 
quit with a goodbye message and if the user types 'b' then the application
will go back to the main starting page. This dynamic is similar throughout
the solver so we will state it once here for clarity (the only difference 
being of course what 'b' actually goes back to). Then finally, based on what 
the user inputs it will take them to the solver_helper function to pick out 
a solver, except with a different dimension variable. It also has error handling
so if the user does not type in a correct dimension (i.e. either 1 or 2), 
a message will appear telling the user what to actually type in.*)
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


(*[main ()] is the initial screen for our application. It has instructions of 
how to quit and go back in our application, and also prompts the user to start 
the process of selecting their options. Note that if the user tries to 
go back from this phase by using the command 'b', it will give them a warning
saying that they can't go back from here as this is the starting screen.*)
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
  | "q" -> print_thank_you 1; Stdlib.exit 0;
  | "b" -> 
    ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\nYou can't go back from the starting page!"; 
    main ();
  | x -> dimension_starter 1

let () = main ()