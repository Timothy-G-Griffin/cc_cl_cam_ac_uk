open Ast 
open Jargon 
(**************************************
Compiler Construction 2019
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
let emit_x86 e =
    (* strip ".slang" off of filename *)
    let base_name = String.sub Option.infile 0 ((String.length Option.infile) - 6)
			     
    in let out_chan = open_out (base_name ^ ".s")
			       
    in let tab(c) = output_string out_chan ("\t" ^ c ^ "\n")
				     
    in let cmd c comment =
	    match comment with
	    | None -> tab c
	    | Some s ->
	       ((*want comments to line up nicely! *)
		 let l = String.length c in
		 let tab_string = if l < 8
				 then "\t\t\t"
				 else if l < 16
                		 then "\t\t"
				 else "\t" 	
	         in output_string out_chan ("\t" ^ c ^ tab_string ^ "# " ^ s ^"\n"))
		 	 
    in let label l = output_string out_chan (l ^ ":\n")
				      
    in let unary = function
	 | NOT -> Errors.complain "NOT: not yet implemented in x86"
				  
	 | NEG -> Errors.complain "NEG: not yet implemented in x86"
				  
	 | READ -> (cmd "popq %rdi"    (Some "BEGIN read, put arg in %rdi");
		    cmd "movq $0,%rax" (Some "signal no floating point args");
		    cmd "pushq %r11"   (Some "%r11 is caller-saved "); 
		    cmd "call read"    (Some "get user input");
		    cmd "popq %r11"    (Some "restore %r11"); 		    
		    cmd "pushq %rax"   (Some "END read, a C-call, so result in %rax \n"));

    in let eq () =(let l1 = new_label () in (* label for not = *) 
		   let l2  = new_label () in  (* label for exit *) 
		  (cmd "popq %rax"         (Some "BEGIN EQ, pop into %rax");
                   cmd "popq %r10"         (Some "pop into %r10");
		   cmd "cmp %r10,%rax"     (Some "compare values");
		   cmd ("jne " ^ l1)       (Some "jump if not equal");
		   cmd "pushq $1"          (Some "push true");
		   cmd ("jmp " ^ l2)       (Some "jump to exit label");
		   label l1;
		   cmd "pushq $0"          (Some "END EQ, push false \n");
		   label l2))
		    
    in let binary = function
	 | AND -> Errors.complain "AND: not yet implemented in x86"
				  
	 | OR -> Errors.complain "OR: not yet implemented in x86"
				 
	 | LT -> (let l1 = new_label () in    (* label for not < *) 
		   let l2  = new_label () in  (* label for exit *) 
		  (cmd "popq %rax"         (Some "BEGIN equal, pop into %rax");
                   cmd "popq %r10"         (Some "pop into %r10");
		   cmd "cmp %rax,%r10"     (Some "compare values");
		   cmd ("jge " ^ l1)       (Some "jump if not(%r10 < %rax) = %rax >= %r10");
		   cmd "pushq $1"          (Some "push true");
		   cmd ("jmp " ^ l2)       (Some "jump to exit label");
		   label l1;
		   cmd "pushq $0"          (Some "END EQI, push false \n");
		   label l2))
		   
	 | EQB -> eq ()
		     
	 | EQI -> eq ()
		     
	 | ADD -> (cmd "popq %rax"         (Some "BEGIN add, pop top-of-stack to %rax"); 
		   cmd "addq %rax,(%rsp)"  (Some "END add, add %rax to top-of-stack \n"))
		    
	 | SUB -> (cmd "popq %rax"         (Some "BEGIN sub, pop top-of-stack to %rax"); 
		   cmd "subq %rax,(%rsp)"  (Some "END sub, subtract %rax from top-of-stack \n"))
		    
	 | MUL -> (cmd "popq %rax"         (Some "BEGIN mul, pop arg 1 to %rax");
		   cmd "popq %r10"         (Some "pop arg 2 to %r10"); 
		   cmd "imulq %r10"        (Some "multiply %r10 by %rax, result in %rax"); 
		   cmd "pushq %rax"        (Some "END mul, push result \n"))
		    
	 | DIV -> (cmd "popq %r10"         (Some "BEGIN div, , pop top-of-stack to %r10");
		   cmd "popq %rax"         (Some "pop divisor into %rax"); 
		   cmd "cqto"              (Some "prepare for div (read x86 docs!)");		   
		   cmd "idivq %r10"        (Some "do the div, result in %rax"); 
		   cmd "pushq %rax"        (Some "END div, push result \n"))
	   
    in let mkpair () =
	 (cmd "movq %r11,%rdi"        (Some "BEGIN make pair, alloc arg 1 in %rdi"); 
	  cmd "movq $2,%rsi"          (Some "alloc arg 2 in %rsi");
	  cmd "movq $0,%rax"          (Some "signal no floating point args");
	  cmd "pushq %r11"            (Some "%r11 is caller-saved "); 
	  cmd "call alloc"            (Some "C-call, so result in %rax");
	  cmd "popq %r11"             (Some "restore %r11"); 		    	  
	  cmd "popq %r10"             (Some "pop element 2 into %r10");	  
	  cmd "movq %r10,8(%rax)"     (Some "copy element 2 to heap");
	  cmd "popq %r10"             (Some "pop element 1 into %r10");	  	  
	  cmd "movq %r10,(%rax)"      (Some "copy element 1 to heap");
	  cmd "pushq %rax"            (Some "END make pair, push heap pointer on stack \n"))
	 
    in let fst () = (cmd "movq (%rsp),%rax"  (Some "BEGIN FST, copy heap pointer");
		     cmd "movq (%rax),%r10"  (Some "copy element 1 to scratch register");
		     cmd "movq %r10,(%rsp)"  (Some "END FST, replace top-of-stack with element 1 \n"))
		      
    in let snd () = (cmd "movq (%rsp),%rax"  (Some "BEGIN SND, copy heap pointer");
		     cmd "movq 8(%rax),%r10" (Some "copy element 2 to scratch register");
		     cmd "movq %r10,(%rsp)"  (Some "END SND, replace top-of-stack with element 2 \n"))
		      
    in let mkinl () = 
	 (cmd "movq %r11,%rdi"        (Some "BEGIN make inl, alloc arg 1 in %rdi"); 
	  cmd "movq $2,%rsi"          (Some "alloc arg 2 in %rsi");
	  cmd "movq $0,%rax"          (Some "signal no floating point args");
	  cmd "pushq %r11"            (Some "%r11 is caller-saved "); 
	  cmd "call alloc"            (Some "... result in %rax");
	  cmd "popq %r11"             (Some "restore %r11"); 		    	  	  
	  cmd "movq $0,(%rax)"        (Some "copy inl tag to the heap");
	  cmd "popq %r10"             (Some "pop argument into %r10");	  
	  cmd "movq %r10,8(%rax)"     (Some "copy argument to the heap");
	  cmd "pushq %rax"            (Some "END make inl, push heap pointer \n"))

   in let mkinr () =
	 (cmd "movq %r11,%rdi"        (Some "BEGIN make inr, alloc is a C call, arg 1 in %rdi");
	  cmd "movq $2,%rsi"          (Some "arg 2 in %rsi");
	  cmd "movq $0,%rax"          (Some "signal no floating point args");
	  cmd "pushq %r11"            (Some "%r11 is caller-saved "); 
	  cmd "call alloc"            (Some "... result in %rax");
	  cmd "popq %r11"             (Some "restore %r11"); 		    	  	  
	  cmd "movq $1,(%rax)"        (Some "copy inr tag to the heap");
	  cmd "popq %r10"             (Some "pop argument into %r10");	  	  
	  cmd "movq %r10,8(%rax)"     (Some "copy argument to the heap");
	  cmd "pushq %rax"            (Some "END make inr, push heap pointer \n"))

   in let case l =
	(cmd "popq %rax"            (Some "BEGIN case, pop heap pointer into %rax");
	 cmd "movq 8(%rax),%r10"    (Some "get the value");
	 cmd "pushq %r10"           (Some "push the value"); 	 	 
	 cmd "movq (%rax),%r10"     (Some "get tag"); 
	 cmd "cmpq $0,%r10"         (Some "compare tag to inl tag"); 
	 cmd ("jne " ^ l)           (Some "END case, jump if not equal \n"))

  in let stack_lookup i =
      (* pointers in x86-land are at byte-level, so for every word advanced
         need to multiply by 8 (number of bytes in 64-bit word *)	    
	(let j = string_of_int (8 * i) in  	   	    
         (cmd ("movq " ^ j ^ "(%rbp)" ^ ",%r10") (Some "BEGIN stack lookup, index off of base pointer");
	  cmd ("pushq %r10")                     (Some "END stack lookup, push value \n")))
	                        
   in let heap_lookup i =
	(let j = string_of_int (8 * i) in  	   
	 (cmd "movq 8(%rbp), %rax"               (Some "BEGIN heap lookup, copy closure pointer to %rax");
 	  cmd ("movq " ^ j ^ "(%rax)" ^ ",%r10") (Some ("put closue value at index " ^ (string_of_int i) ^ " in scratch register"));
	  cmd "pushq %r10"                       (Some "END heap lookup, push value \n")))

   in let test l = 
	(cmd "popq %rax"            (Some "BEGIN test, pop stack-top into %rax");
	 cmd "cmp $1,%rax"          (Some "compare to value of true"); 
	 cmd ("jne " ^ l)           (Some "END test, jump if not equal \n"))     

   in let goto l = cmd ("jmp " ^ l) None
			  
   in let swap () =
	 (cmd "movq (%rsp),%rax"      (Some "BEGIN swap");
	  cmd "movq 8(%rsp),%r10"      None;
	  cmd "movq %r10,(%rsp)"       None;	  
	  cmd "movq %rax,8(%rsp)"     (Some "END swap \n"))
	   
   in let mkref () =
	 (cmd "movq %r11,%rdi"        (Some "BEGIN make ref, alloc arg 1 in %rdi");        
	  cmd "movq $1,%rsi"          (Some "alloc arg 2 in %rsi");
	  cmd "movq $0,%rax"          (Some "signal no floating point args");
	  cmd "pushq %r11"            (Some "%r11 is caller-saved "); 
	  cmd "call alloc"            (Some "alloc is a C-call, result in %rax");
	  cmd "popq %r11"             (Some "restore %r11"); 		    	  	  
	  cmd "popq %r10"             (Some "copy value into scratch register"); 	  
	  cmd "movq %r10,(%rax)"      (Some "copy value to heap"); 
	  cmd "pushq %rax"            (Some "END make ref, push heap pointer \n"))
	   
    in let deref () =
	 (cmd "movq (%rsp),%rax"      (Some "BEGIN deref, copy ref pointer to $aux");
   	  cmd "movq (%rax),%rax"      (Some "copy value to %rax"); 
   	  cmd "movq %rax,(%rsp)"      (Some "END deref, replace top-of-stack with value \n"))
	   
    in let assign () =
	 (cmd "popq %rax"          (Some "BEGIN assign, pop value into %rax");
	  cmd "movq %rax,(%rsp)"   (Some "copy value to ref cell in heap");
      	  cmd "movq $0,(%rsp)"     (Some "END assign, replace heap pointer with unit \n"))

    in let closure(l, n) =
	(let m = string_of_int (n + 1) in 
	 (cmd "movq %r11,%rdi"                   (Some "BEGIN make closure, alloc arg 1 in %rdi"); 
	  cmd ("movq $" ^ m ^ ",%rsi")           (Some "arg 2 to alloc in %rsi");
	  cmd "movq $0,%rax"                     (Some "signal no floating point args");
	  cmd "pushq %r11"                       (Some "%r11 is caller-saved ");	  
	  cmd "call alloc"                       (Some "... result in %rax");
	  cmd "popq %r11"                        (Some "restore %r11"); 		    	  	  
	  cmd ("leaq " ^ l ^ "(%rip)" ^ ",%r10") (Some "place code address in scratch register");
	  cmd ("movq %r10,(%rax)")               (Some "place code address in heap closure");
   	  for i = 1 to n do
	    let j = string_of_int (8 * i) in  
	    (cmd ("popq %r10")                   (Some "pop value into the scratch register");
	     cmd ("movq %r10," ^ j ^ "(%rax)")   (Some "copy value to the heap"))
	  done; 
	  cmd "pushq %rax"                       (Some "END make closure, push heap pointer returned by alloc \n")))
	      
	      
    in let apply () =
	    (cmd "movq (%rsp),%rax"   (Some "BEGIN apply, copy closure pointer to %rax");
             cmd "movq (%rax),%rax"   (Some "get the the function address from heap");	  
	     cmd "pushq %rbp"         (Some "save the frame pointer");
	     cmd "movq %rsp,%rbp"     (Some "set new frame pointer");	     	     
      	     cmd "call *%rax"         (Some "call pushes return address, jumps to function");
	     cmd "popq %rbp"          (Some "retore base pointer");	     
	     cmd "addq $8, %rsp"      (Some "pop closure");
	     cmd "addq $8, %rsp"      (Some "pop argument");
	     cmd "pushq %rax"         (Some "END apply, push returned value on stack \n"))
	      
    in let ret () = 
	    (cmd "popq %rax"         (Some "BEGIN return. put top-of-stack in %rax");
      	     cmd "ret"               (Some "END retrun, this pops return address, jumps there \n"))
	    
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
	  | POP                     -> cmd "addq $8, %rsp" (Some "pop stack \n")
	  | PUSH (STACK_INT i)      -> cmd ("pushq $" ^ (string_of_int i)) (Some "push int \n")
	  | PUSH (STACK_BOOL true)  -> cmd "pushq $1" (Some "push true \n")
	  | PUSH (STACK_BOOL false) -> cmd "pushq $0" (Some "push false \n")
	  | PUSH STACK_UNIT         -> cmd "pushq $0" (Some "push false \n")
	  | PUSH (STACK_HI i)       -> Errors.complain "Internal Error : Jargon code never explicitly pushes stack pointer"
	  | PUSH (STACK_RA i)       -> Errors.complain "Internal Error : Jargon code never explicitly pushes return address"
	  | PUSH (STACK_FP i)       -> Errors.complain "Internal Error : Jargon code never explicitly pushes frame pointer"
	  | HALT                    -> Errors.complain "HALT found in Jargon code from Jargon.comp"

    in let rec emitl = function [] -> () | c::l -> (emitc c; emitl l)

    in let do_command s = if 0 = Sys.command s then () else Errors.complain ("command failed: " ^ s) 
    
    in let (defs, cl) = comp [] e           (* compile to Jargon code with Jargon.comp  *) 						     
       in (* emit header *)
       (tab ".text";
        tab ".extern alloc" ;
	tab ".extern read";
	tab ".globl giria";
	tab ".type giria, @function";

	output_string out_chan "giria:\n";  (* label for main body of slang program *)
	
	cmd "pushq %rbp"	(Some "BEGIN giria : save base pointer"); 
	cmd "movq %rsp,%rbp"    (Some "BEGIN giria : set new base pointer");
	cmd "movq %rdi,%r11"    (Some "BEGIN giria : save pointer to heap in %r11 \n");
	
	emitl cl;               (* main body of program *)
	
	cmd "popq %rax"         (Some "END giria : place return value in %rax"); 
	cmd "movq %rbp,%rsp"	(Some "END giria : reset stack to previous base pointer");   
	cmd "popq %rbp"	        (Some "END giria : restore base pointer");
	cmd "ret"               (Some "END giria : return to runtime system \n");

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
