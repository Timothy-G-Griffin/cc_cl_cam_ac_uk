(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 

(*  This is the main file. *) 

(* 
   parse command line options and args 
*) 
let infile         = ref ""
let verbose        = ref false
let verbose_front  = ref false
let run_tests      = ref false
let use_i0         = ref false
let set_infile f   = infile := f 

let option_spec = [
     ("-V",    Arg.Set verbose_front, "verbose front end"); 
     ("-i0",   Arg.Set use_i0,        "Interpreter 0 (more later ...)"); 
     ("-t",    Arg.Set run_tests,     "run all test/*.slang with each selected interpreter, report unexpected outputs (silent otherwise)")
    ] 
let usage_msg = "Usage: slang.byte [options] [<file>]\nOptions are:"

let _ = Arg.parse option_spec set_infile usage_msg

(* set all verbosity flags *) 
let _ = if !verbose_front then Front_end.verbose := true else () 

let error file action s = print_string ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n")

let fatal_error file action s = let _ = error file action s in exit(-1) 

(* bind interpreters *) 
(* each interpreter i_ has type "(string * Ast.expr) -> string option" *) 

let wrap file e interpret msg = 
    try Some(interpret e)
    with Errors.Error s -> let _ = error file msg s in None 
       | exc -> fatal_error file msg ("Exception: " ^ (Printexc.to_string exc))

let i0 (file, e)   = wrap file e (fun x -> Interp_0.string_of_value (Interp_0.interpret_top_level x)) "Interpreter 0" 
let interpreters = [
    (* use-flag, the interpreter, a description string *) 
    (!use_i0,       i0,   "Interpreter 0")] 

let show_output describe string_out = 
    if !run_tests
    then () 
    else let _ = if !verbose then print_string ("\n" ^ describe ^ " : \n") else ()
         in print_string ("output> " ^ string_out ^ "\n")

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
let rec process_inputs = function 
       | [] -> () 
       | (file, expected) :: rest -> 
          let e_opt = try Some (Front_end.front_end file)
                      with Errors.Error s -> let _ = error file "Front End" s in None  
          in let _ = match e_opt with 
                    | Some e ->  run_interpreters file e expected interpreters 
                    | None -> () 
          in process_inputs rest 
 
let _ = process_inputs 
        (if !run_tests 
        then (try Tests.get_all_tests () 
              with Errors.Error s -> fatal_error "tests/" "Test.get_all_tests" s)
        else [(!infile, None)])
            




