(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(*  This is the main file. *)

let error file action s = print_string ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n")

let fatal_error file action s =
   error file action s;
   exit(-1)

(* bind interpreters *)
(* each interpreter i_ has type "(string * Ast.expr) -> string option" *)

let wrap file e interpret msg =
    try Some(interpret e)
    with
        | Errors.Error s ->
            error file msg s;
            None
        | exc -> fatal_error file msg ("Exception: " ^ (Printexc.to_string exc))

let i0 (file, e) = wrap file e (fun x -> Interp_0.string_of_value (Interp_0.interpret_top_level x)) "Interpreter 0"

let interpreters = [
   (* use-flag, the interpreter, a description string *)
   (Option.use_i0, i0, "Interpreter 0");
]

let show_output describe string_out =
    if Option.run_tests
    then ()
    else begin
       (if Option.verbose
       then print_string ("\n" ^ describe ^ " : \n")
       else ()
       );
       print_string ("output> " ^ string_out ^ "\n")
    end

(* used for -t option *)
let check_expected describe file string_out = function
  | None -> ()
  | Some expected ->
      if string_out = expected
      then ()
      else error file "testing" (
              describe ^ " computed " ^ string_out
	      ^ ", but expected " ^ expected)

(* runs all interpreters on expression e : Ast.expr.
   If expected_option = Some (expected), then
   checks if the correct result was computed (silent otherwise).
*)
let rec run_interpreters file e expected_option = function
    | [] -> ()
    | (use, interp, describe) :: rest ->
      if use
      then (match interp(file, e) with
           | None -> run_interpreters file e expected_option rest
           | Some string_out ->
              let _ = show_output describe string_out
              in let _ = check_expected describe file string_out expected_option
              in run_interpreters file e expected_option rest )
      else run_interpreters file e expected_option rest

(* process_inputs : runs all (flagged) interpreters on all inputs *)
let process_input file expected =
   try
      let e = Front_end.front_end file in
      run_interpreters file e expected interpreters
   with
      Errors.Error s -> error file "Front_end" s
let rec process_inputs = function
   | [] -> ()
   | (file, expected) :: rest ->
         process_input file expected;
         process_inputs rest

let _ = process_inputs
        (if Option.run_tests
        then (try Tests.get_all_tests ()
              with Errors.Error s -> fatal_error "tests/" "Test.get_all_tests" s)
        else [(Option.infile, None)])





