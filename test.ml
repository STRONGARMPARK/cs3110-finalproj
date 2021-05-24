open OUnit2
open Graphs
open Graphs2d

(* GRAPH TESTS *)

let print_v_int (v : int * int) =
  "( " ^ string_of_int (fst v) ^ " , " ^ string_of_int (snd v) ^ " )"

let print_v_float (v : float * float) =
  "( "
  ^ string_of_float (fst v)
  ^ " , "
  ^ string_of_float (snd v)
  ^ " )"

let test_vop name op a b expected printer =
  name >:: fun _ -> assert_equal expected (Graphs.vop op a b) ~printer

let test_vfloat name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.vfloat a) ~printer:print_v_float

let test_vint name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.vint a) ~printer:print_v_int

let test_mag_float name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.mag_float a) ~printer:(fun x ->
      string_of_float x)

let test_mag_int name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.mag_int a) ~printer:(fun x ->
      string_of_float x)

let graphs_tests =
  [
    test_vop "2d point addition operation with ints" ( + ) (1, 1) (0, 2)
      (1, 3) print_v_int;
    test_vop "2d point addition operation with floats" ( +. ) (4., 2.)
      (2., 3.) (6., 5.) print_v_float;
    test_vop "2d point addition operation with ints" ( * ) (1, 1) (0, 2)
      (0, 2) print_v_int;
    test_vop "2d point addition operation with floats" ( *. ) (4., 2.)
      (2., 3.) (8., 6.) print_v_float;
    test_vfloat "2d point int to float" (2, 2) (2., 2.);
    test_vint "2d point float to int" (2., 2.) (2, 2);
    test_mag_float "magnitude of 2d float point (4.,3.)" (4., 3.) 5.;
    test_mag_int "magnitude of 2d int point (4,3)" (4, 3) 5.;
  ]

let suite =
  "test suite for OCAML final proj" >::: List.flatten [ graphs_tests ]

let _ = run_test_tt_main suite
