open Ast 
open Jargon 
(**************************************
Compiler Construction 2020
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)

This file contains a VERY simple translation of Jargon to x86 assembler. 
The generated code attempts to closely mimic the Jargon virtual machine. 
Calling slang with the -i4x86 option and file foo.slang  will generate 
a foo.s file of assembler (with comments!), and a compiled version 
"foo" that you can run from the command line (foo.s is compiled and
linked with the runtime system (runtime/c_runtime.c). 

Thanks for help from Nathan Corbyn who implemented 
rusty_slang here https://github.com/doctorn/rusty_slang. 

A few comments on the code below: 

-- No optimisations have been implemented. 
-- No garbage collection is implemented. Heap allocated 
   records do not yet have headers. 
-- runtime system only prints out integers correctly. 
   This is related to having no headers. Discuss. 
-- the scratch register %r11 is used to hold a pointer to 
   the heap, which is passed in by the runtime system. 
-- register %rax is being used for scratch, not really as an accumulator. 
-- Close inspection of generated code will reveal why it would be good idea
   to use %rax as an accumulator. That is, always have to top-of-stack in %rax. 
-- The only function calls/returns that follow the C calling 
   conventions are those that interact with the runtime system
   (the "alloc" function is called to allocate records on the heap, 
   and the main function "giria" is called from the runtime system).
-- "giria" means "slang" in Portuguese. 

 *****************************************)
let complain = Errors.complain 
  
let emit_x86 e =
    (* strip ".slang" off of filename *)
    let base_name = String.sub Option.infile 0 ((String.length Option.infile) - 6)
			     
    in let out_chan = open_out (base_name ^ ".s")
			       
    in let tab(c) = output_string out_chan ("\t" ^ c ^ "\n")
				     
    in let cmd c comment =
	       ((*want comments to line up nicely! *)
		 let l = String.length c in
		 let tab_string = if l < 8
				 then "\t\t\t"
				 else if l < 16
                		 then "\t\t"
				 else "\t" 	
	         in output_string out_chan ("\t" ^ c ^ tab_string ^ "# " ^ comment ^"\n"))
		 	 
    in let label l = output_string out_chan (l ^ ":\n")
				      
    in let unary = function
	 | NOT -> complain "NOT: not yet implemented in x86"
				  
	 | NEG -> complain "NEG: not yet implemented in x86"
				  
	 | READ -> (cmd "popq %rdi"    "BEGIN read, put arg in %rdi";
		    cmd "movq $0,%rax" "signal no floating point args";
		    cmd "pushq %r11"   "%r11 is caller-saved "; 
		    cmd "call read"    "get user input";
		    cmd "popq %r11"    "restore %r11"; 		    
		    cmd "pushq %rax"   "END read, a C-call, so result in %rax \n");

    in let eq () =(let l1 = new_label () in  (* label for not = *) 
		   let l2 = new_label () in  (* label for exit *) 
		  (cmd "popq %rax"         "BEGIN EQ, pop into %rax";
                   cmd "popq %r10"         "pop into %r10";
		   cmd "cmp %r10,%rax"     "compare values";
		   cmd ("jne " ^ l1)       "jump if not equal";
		   cmd "pushq $1"          "push true";
		   cmd ("jmp " ^ l2)       "jump to exit label";
		   label l1;
		   cmd "pushq $0"          "END EQ, push false \n";
		   label l2))
		    
    in let binary = function
	 | AND -> complain "AND: not yet implemented in x86"
				  
	 | OR -> complain "OR: not yet implemented in x86"
				 
	 | LT -> (let l1 = new_label () in  (* label for not < *) 
		  let l2 = new_label () in  (* label for exit *) 
		  (cmd "popq %rax"         "BEGIN equal, pop into %rax";
                   cmd "popq %r10"         "pop into %r10";
		   cmd "cmp %rax,%r10"     "compare values";
		   cmd ("jge " ^ l1)       "jump if not(%r10 < %rax) = %rax >= %r10";
		   cmd "pushq $1"          "push true";
		   cmd ("jmp " ^ l2)       "jump to exit label";
		   label l1;
		   cmd "pushq $0"          "END EQI, push false \n";
		   label l2))
		   
	 | EQB -> eq ()
		     
	 | EQI -> eq ()
		     
	 | ADD -> (cmd "popq %rax"         "BEGIN add, pop top-of-stack to %rax"; 
		   cmd "addq %rax,(%rsp)"  "END add, add %rax to top-of-stack \n")
		    
	 | SUB -> (cmd "popq %rax"         "BEGIN sub, pop top-of-stack to %rax"; 
		   cmd "subq %rax,(%rsp)"  "END sub, subtract %rax from top-of-stack \n")
		    
	 | MUL -> (cmd "popq %rax"         "BEGIN mul, pop arg 1 to %rax";
		   cmd "popq %r10"         "pop arg 2 to %r10"; 
		   cmd "imulq %r10"        "multiply %r10 by %rax, result in %rax"; 
		   cmd "pushq %rax"        "END mul, push result \n")
		    
	 | DIV -> (cmd "popq %r10"         "BEGIN div, pop top-of-stack to %r10";
		   cmd "popq %rax"         "pop divisor into %rax"; 
		   cmd "cqto"              "prepare for div (read x86 docs!)";		   
		   cmd "idivq %r10"        "do the div, result in %rax"; 
		   cmd "pushq %rax"        "END div, push result \n")
	   
    in let mkpair () =
	 (cmd "movq %r11,%rdi"        "BEGIN make pair, alloc arg 1 in %rdi"; 
	  cmd "movq $2,%rsi"          "alloc arg 2 in %rsi";
	  cmd "movq $0,%rax"          "signal no floating point args";
	  cmd "pushq %r11"            "%r11 is caller-saved "; 
	  cmd "call alloc"            "C-call, so result in %rax";
	  cmd "popq %r11"             "restore %r11"; 		    	  
	  cmd "popq %r10"             "pop element 2 into %r10";	  
	  cmd "movq %r10,8(%rax)"     "copy element 2 to heap";
	  cmd "popq %r10"             "pop element 1 into %r10";	  	  
	  cmd "movq %r10,(%rax)"      "copy element 1 to heap";
	  cmd "pushq %rax"            "END make pair, push heap pointer on stack \n")
	 
    in let fst () = (cmd "movq (%rsp),%rax"  "BEGIN FST, copy heap pointer";
		     cmd "movq (%rax),%r10"  "copy element 1 to scratch register";
		     cmd "movq %r10,(%rsp)"  "END FST, replace top-of-stack with element 1 \n")
		      
    in let snd () = (cmd "movq (%rsp),%rax"  "BEGIN SND, copy heap pointer";
		     cmd "movq 8(%rax),%r10" "copy element 2 to scratch register";
		     cmd "movq %r10,(%rsp)"  "END SND, replace top-of-stack with element 2 \n")
		      
    in let mkinl () = 
	 (cmd "movq %r11,%rdi"        "BEGIN make inl, alloc arg 1 in %rdi"; 
	  cmd "movq $2,%rsi"          "alloc arg 2 in %rsi";
	  cmd "movq $0,%rax"          "signal no floating point args";
	  cmd "pushq %r11"            "%r11 is caller-saved "; 
	  cmd "call alloc"            "... result in %rax";
	  cmd "popq %r11"             "restore %r11"; 		    	  	  
	  cmd "movq $0,(%rax)"        "copy inl tag to the heap";
	  cmd "popq %r10"             "pop argument into %r10";	  
	  cmd "movq %r10,8(%rax)"     "copy argument to the heap";
	  cmd "pushq %rax"            "END make inl, push heap pointer \n")

   in let mkinr () =
	 (cmd "movq %r11,%rdi"        "BEGIN make inr, alloc is a C call, arg 1 in %rdi";
	  cmd "movq $2,%rsi"          "arg 2 in %rsi";
	  cmd "movq $0,%rax"          "signal no floating point args";
	  cmd "pushq %r11"            "%r11 is caller-saved "; 
	  cmd "call alloc"            "... result in %rax";
	  cmd "popq %r11"             "restore %r11"; 		    	  	  
	  cmd "movq $1,(%rax)"        "copy inr tag to the heap";
	  cmd "popq %r10"             "pop argument into %r10";	  	  
	  cmd "movq %r10,8(%rax)"     "copy argument to the heap";
	  cmd "pushq %rax"            "END make inr, push heap pointer \n")

   in let case l =
	(cmd "popq %rax"            "BEGIN case, pop heap pointer into %rax";
	 cmd "movq 8(%rax),%r10"    "get the value";
	 cmd "pushq %r10"           "push the value"; 	 	 
	 cmd "movq (%rax),%r10"     "get tag"; 
	 cmd "cmpq $0,%r10"         "compare tag to inl tag"; 
	 cmd ("jne " ^ l)           "END case, jump if not equal \n")

  in let stack_lookup i =
      (* pointers in x86-land are at byte-level, so for every word advanced
         need to multiply by 8 (number of bytes in 64-bit word *)	    
	(let j = string_of_int (8 * i) in  	   	    
         (cmd ("movq " ^ j ^ "(%rbp)" ^ ",%r10") "BEGIN stack lookup, index off of base pointer";
	  cmd "pushq %r10"                       "END stack lookup, push value \n"))
	                        
   in let heap_lookup i =
	(let j = string_of_int (8 * i) in  	   
	 (cmd "movq 8(%rbp), %rax"               "BEGIN heap lookup, copy closure pointer to %rax";
 	  cmd ("movq " ^ j ^ "(%rax)" ^ ",%r10") ("put closure value at index " ^ (string_of_int i) ^ " in scratch register");
	  cmd "pushq %r10"                       "END heap lookup, push value \n"))

   in let test l = 
	(cmd "popq %rax"            "BEGIN test, pop stack-top into %rax";
	 cmd "cmp $1,%rax"          "compare to value of true"; 
	 cmd ("jne " ^ l)           "END test, jump if not equal \n")

   in let goto l = cmd ("jmp " ^ l) ""
			  
   in let swap () =
	 (cmd "movq (%rsp),%rax"      "BEGIN swap";
	  cmd "movq 8(%rsp),%r10"      "";
	  cmd "movq %r10,(%rsp)"       "";	  
	  cmd "movq %rax,8(%rsp)"     "END swap \n")
	   
   in let mkref () =
	 (cmd "movq %r11,%rdi"        "BEGIN make ref, alloc arg 1 in %rdi";        
	  cmd "movq $1,%rsi"          "alloc arg 2 in %rsi";
	  cmd "movq $0,%rax"          "signal no floating point args";
	  cmd "pushq %r11"            "%r11 is caller-saved "; 
	  cmd "call alloc"            "alloc is a C-call, result in %rax";
	  cmd "popq %r11"             "restore %r11"; 		    	  	  
	  cmd "popq %r10"             "copy value into scratch register"; 	  
	  cmd "movq %r10,(%rax)"      "copy value to heap"; 
	  cmd "pushq %rax"            "END make ref, push heap pointer \n")
	   
    in let deref () =
	 (cmd "movq (%rsp),%rax"      "BEGIN deref, copy ref pointer to $rax";
   	  cmd "movq (%rax),%rax"      "copy value to %rax"; 
   	  cmd "movq %rax,(%rsp)"      "END deref, replace top-of-stack with value \n")
	   
    in let assign () =
	 (cmd "popq %rax"          "BEGIN assign, pop value into %rax";
	  cmd "movq %rax,(%rsp)"   "copy value to ref cell in heap";
      	  cmd "movq $0,(%rsp)"     "END assign, replace heap pointer with unit \n")

    in let closure(l, n) =
	(let m = string_of_int (n + 1) in 
	 (cmd "movq %r11,%rdi"                   "BEGIN make closure, alloc arg 1 in %rdi"; 
	  cmd ("movq $" ^ m ^ ",%rsi")           "arg 2 to alloc in %rsi";
	  cmd "movq $0,%rax"                     "signal no floating point args";
	  cmd "pushq %r11"                       "%r11 is caller-saved ";	  
	  cmd "call alloc"                       "... result in %rax";
	  cmd "popq %r11"                        "restore %r11"; 		    	  	  
	  cmd ("leaq " ^ l ^ "(%rip)" ^ ",%r10") "place code address in scratch register";
	  cmd ("movq %r10,(%rax)")               "place code address in heap closure";
   	  for i = 1 to n do
	    let j = string_of_int (8 * i) in  
	    (cmd ("popq %r10")                   "pop value into the scratch register";
	     cmd ("movq %r10," ^ j ^ "(%rax)")   "copy value to the heap")
	  done; 
	  cmd "pushq %rax"                       "END make closure, push heap pointer returned by alloc \n"))
	      
	      
    in let apply () =
	    (cmd "movq (%rsp),%rax"   "BEGIN apply, copy closure pointer to %rax";
             cmd "movq (%rax),%rax"   "get the the function address from heap";	  
	     cmd "pushq %rbp"         "save the base pointer";
	     cmd "movq %rsp,%rbp"     "set new base pointer";	     	     
      	     cmd "call *%rax"         "call pushes return address, jumps to function";
	     cmd "popq %rbp"          "restore base pointer";	     
	     cmd "addq $8, %rsp"      "pop closure";
	     cmd "addq $8, %rsp"      "pop argument";
	     cmd "pushq %rax"         "END apply, push returned value on stack \n")
	      
    in let ret () = 
	    (cmd "popq %rax"         "BEGIN return. put top-of-stack in %rax";
      	     cmd "ret"               "END return, this pops return address, jumps there \n")
	    
    (* emit command *) 	    
    in let emitc = function
	  | UNARY op -> unary op 
	  | OPER op  -> binary op 
	  | MK_PAIR  -> mkpair () 
	  | FST      -> fst() 
	  | SND      -> snd()
	  | MK_INL   -> mkinl()
	  | MK_INR   -> mkinr()
	  | MK_REF   -> mkref()
	  | DEREF    -> deref() 
	  | ASSIGN   -> assign()
	  | APPLY    -> apply () 
	  | RETURN   -> ret()
	  | LABEL l  -> label l 
	  | SWAP     -> swap ()
	  | TEST (l, _) -> test l 			     			     
	  | GOTO (l, _) -> goto l 			     
	  | CASE (l, _) -> case l
	  | MK_CLOSURE ((l, _), n)         -> closure(l, n) 				  
	  | LOOKUP (STACK_LOCATION offset) -> stack_lookup (0 - offset) (* stack grows downward, so negate offsets *) 
	  | LOOKUP (HEAP_LOCATION offset)  -> heap_lookup offset			      
	  | POP                     -> cmd "addq $8, %rsp" "pop stack \n"
	  | PUSH (STACK_INT i)      -> cmd ("pushq $" ^ (string_of_int i)) "push int \n"
	  | PUSH (STACK_BOOL true)  -> cmd "pushq $1" "push true \n"
	  | PUSH (STACK_BOOL false) -> cmd "pushq $0" "push false \n"
	  | PUSH STACK_UNIT         -> cmd "pushq $0" "push unit \n"
	  | PUSH (STACK_HI i)       -> complain "Internal Error : Jargon code never explicitly pushes stack pointer"
	  | PUSH (STACK_RA i)       -> complain "Internal Error : Jargon code never explicitly pushes return address"
	  | PUSH (STACK_FP i)       -> complain "Internal Error : Jargon code never explicitly pushes frame pointer"
	  | HALT                    -> complain "HALT found in Jargon code from Jargon.comp"

    in let rec emitl = function [] -> () | c::l -> (emitc c; emitl l)

    in let do_command s = if 0 = Sys.command s then () else complain ("command failed: " ^ s) 
    
    in let (defs, cl) = comp [] e           (* compile to Jargon code with Jargon.comp  *) 						     
       in (* emit header *)
       (tab ".text";
        tab ".extern alloc" ;
	tab ".extern read";
	tab ".globl giria";
	tab ".type giria, @function";

	output_string out_chan "giria:\n";  (* label for main body of slang program *)
	
	cmd "pushq %rbp"	"BEGIN giria : save base pointer"; 
	cmd "movq %rsp,%rbp"    "BEGIN giria : set new base pointer";
	cmd "movq %rdi,%r11"    "BEGIN giria : save pointer to heap in %r11 \n";
	
	emitl cl;               (* main body of program *)
	
	cmd "popq %rax"         "END giria : place return value in %rax"; 
	cmd "movq %rbp,%rsp"	"END giria : reset stack to previous base pointer";   
	cmd "popq %rbp"	        "END giria : restore base pointer";
	cmd "ret"               "END giria : return to runtime system \n";

        emitl defs;             (* the function definitions *)
	
        flush out_chan;
        close_out out_chan;
	
	(* compile and link with runtime.  Comment out these lines if you don't have gcc installed. *)
	do_command "gcc -g -o runtime/c_runtime.o -c runtime/c_runtime.c"; 
        do_command ("gcc -g -o " ^ base_name ^ ".o -c " ^ base_name ^ ".s");
        do_command ("gcc -g -o " ^ base_name ^ " runtime/c_runtime.o " ^ base_name ^ ".o"); 
        do_command ("rm " ^ base_name ^ ".o");
	()
       )
