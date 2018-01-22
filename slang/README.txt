Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 

This directory contains Ocaml code implementing 
high-level interpreters for the language 
Slang ( = Simple LANGuage), a fragment of the 
language L3 from 1B Semantics.   

===============================================
Building 
===============================================
Install ocamlbuild.  Then do either 

  ocamlbuild slang.byte 

or

  ocamlbuild slang.native  

This will build the executable slang.byte (or 
slang.native) as well as a directory _build 
of compiled code. 

To clean up, just do 

  ocamlbuild -clean 

Building slang.byte is quicker, but the runtime of 
slang.native is faster.  However, you probably will 
not notice the difference on this project. 

===============================================
Usage 
===============================================

Usage: slang.byte [options] [<file>]
Options are:
  -V verbose front end
  -v verbose interpreter(s)
  -c show compiled code (but don't run it)
  -i0 Interpreter 0
  -i1 Interpreter 1
  -i2 Interpreter 2
  -i3 Interpreter 3
  -i4 Jargon VM
  -all all interpreters
  -stackmax set max stack size (default = 1000)
  -heapmax set max heap size (default = 1000)
  -t run all test/*.slang with each selected interpreter, report unexpected outputs (silent otherwise)
  -help  Display this list of options
  --help  Display this list of options


===============================================
Files
===============================================

Every .ml file has an associated .mli file describing its interface. 

errors.ml      : Error exception 
past.ml        : the Parsed AST, with pretty printing 
lexer.mll      : specification for ocamllex 
parser.mly     : specification for ocamlyacc 
ast.ml         : "internal" AST, with pretty printing  
past_to_ast.ml : translated from parsed to internal AST 
static.ml      : static analysis (check types and other rules) 
front_end.ml   : the front end : parse, static check, translate. 
free_vars.ml   : free variable calculation 
tests.ml       : code for parsing tests/manifest.txt and setting up testing. 
slang.ml       : main file, implementing the command-line for the interpreter and compiler 

Interpreters (In order of presentation in lectures)
interp_0.ml    : The "definitional" interpreter. 
interp_2.ml    : A high-level stack-oriented abstract machine with compiler. 
                 What do I mean by "high-level"? 
                 ---Code is still tree-structured. 
                 ---Complex values are pushed onto value stack.  
                 ---Heap used only for references. 
                 ---Code is maintained on a code stack. 
                 ---Program variables contained in code. 
interp_3.ml    : A slightly lower-level stack-oriented abstract machine with compiler. 
                 Code is now in a strictly linear array of instructions. 
                 State includes a "code pointer". 
jargon.ml      : Could be called "interp_4". 
                 The Jargon VM and compiler. 
                 Only simple values on the value stack. 
                 Complex values now stored in heap. 
                 Value stack items now have an address. 
interp_1.ml    : The "missing link" between interp_0
                 and interp_2.  Best understood in terms
                 of a CPS translation on interp_0 itself. 



               
