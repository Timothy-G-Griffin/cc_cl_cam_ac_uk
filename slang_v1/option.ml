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

(* This does the parsing and *)
let () = Arg.parse option_spec set_infile usage_msg

(* set immutable versions of the options now that they have been parsed 
 * Note: this is only to make the interface cleaner. *)
let infile        = !infile
let verbose       = !verbose
let verbose_front = !verbose_front
let run_tests     = !run_tests
let use_i0        = !use_i0

