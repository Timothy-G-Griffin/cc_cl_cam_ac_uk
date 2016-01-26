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
let use_i1         = ref false
let use_i2         = ref false
let use_i3         = ref false
let use_i4         = ref false
let use_all        = ref false
let show_compiled  = ref false
let set_infile f   = infile := f 
let set_stack_max m = Jargon.stack_max := m
let set_heap_max m = (Interp_1.heap_max := m; 
                      Interp_2.heap_max := m;
                      Interp_3.heap_max := m;
                      Jargon.heap_max := m 
                      )

let option_spec = [
     ("-V",    Arg.Set verbose_front, "verbose front end"); 
     ("-v",    Arg.Set verbose,       "verbose interpreter(s)"); 
     ("-c",    Arg.Set show_compiled, "show compiled code (but don't run it)"); 
     ("-i0",   Arg.Set use_i0,        "Interpreter 0"); 
     ("-i1",   Arg.Set use_i1,        "Interpreter 1" ); 
     ("-i2",   Arg.Set use_i2,        "Interpreter 2" ); 
     ("-i3",   Arg.Set use_i3,        "Interpreter 3" ); 
     ("-i4",   Arg.Set use_i4,        "Jargon VM" ); 
     ("-all",  Arg.Set use_all,       "all interpreters"); 
     ("-stackmax",  Arg.Int (set_stack_max), "set max stack size (default = 1000)"); 
     ("-heapmax",  Arg.Int (set_heap_max), "set max heap size (default = 1000)"); 
     ("-t",    Arg.Set run_tests,     "run all test/*.slang with each selected interpreter, report unexpected outputs (silent otherwise)")
    ] 
let usage_msg = "Usage: slang.byte [options] [<file>]\nOptions are:"

let _ = Arg.parse option_spec set_infile usage_msg

let _ = if !use_all 
        then (use_i0     := true; 
	      use_i1     := true; 
	      use_i2     := true; 
	      use_i3     := true; 
	      use_i4     := true; 
             )
        else ()

(* set all verbosity flags *) 
let _ = if !verbose_front then Front_end.verbose := true else () 
let _ = if !verbose 
        then (
              Interp_0.verbose := true; 
              Interp_1.verbose := true; 
              Interp_2.verbose := true; 
              Interp_3.verbose := true; 
              Jargon.verbose := true;
             )
        else () 

let error file action s = print_string ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n")

let fatal_error file action s = let _ = error file action s in exit(-1) 

         
(* bind interpreters *) 
(* each interpreter i_ has type "(string * Ast.expr) -> string option" *) 

let wrap file e interpret msg = 
    try Some(interpret e)
    with Errors.Error s -> let _ = error file msg s in None 
       | exc -> let _ = error file msg ("Exception: " ^ (Printexc.to_string exc)) in None 

let i0 (file, e)   = wrap file e (fun x -> Interp_0.string_of_value (Interp_0.interpret_top_level x)) "Interpreter 0" 
let i1 (file, e)   = wrap file e (fun x -> Interp_1.string_of_value (Interp_1.interpret x)) "Interpreter 1" 
let i2 (file, e)   = wrap file e (fun x -> Interp_2.string_of_value (Interp_2.interpret x)) "Interpreter 2" 
let i3 (file, e)   = wrap file e (fun x -> Interp_3.string_of_value (Interp_3.interpret x)) "Interpreter 3" 
let i4 (file, e)   = wrap file e (fun x -> Jargon.string_of_value (Jargon.interpret x)) "Jargon VM" 

(* show compiled code *) 
let i2cc (file, e)   = let _ = print_string (Interp_2.string_of_code (Interp_2.compile e)) in None 
let i3cc (file, e)   = let _ = print_string (Interp_3.string_of_code (Interp_3.compile e)) in None 
let i4cc (file, e)   = let _ = print_string (Jargon.string_of_listing (Jargon.compile e)) in None 


let interpreters = [
    (* use-flag,  the action, a description string *) 
    (!use_i0,                        i0,   "Interpreter 0");
    (!use_i1,                        i1,   "Interpreter 1");
    (!use_i2 && not(!show_compiled), i2,   "Interpreter 2");
    (!show_compiled && !use_i2     , i2cc, "Interpreter 2, compiled code");
    (!use_i3 && not(!show_compiled), i3,   "Interpreter 3"); 
    (!show_compiled && !use_i3     , i3cc, "Interpreter 3, compiled code");
    (!use_i4 && not(!show_compiled), i4,   "Jargon VM"    ); 
    (!show_compiled && !use_i4     , i4cc, "Jargon, compiled code")
] 

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
            




