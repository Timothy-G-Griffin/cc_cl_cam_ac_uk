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
Files
===============================================

Every .ml file has an associated .mli file describing its interface. 

errors.ml      : for generating error messages 
past.ml        : the parsed AST, with pretty printing 
lexer.mll      : specification for ocamllex 
parser.mly     : specification for ocamlyacc 
ast.ml         : "internal" AST, with pretty printing  
past_to_ast.ml : translated from parsed to internal AST 
static.ml      : static analysis (check types and other rules) 
front_end.ml   : the front end : parse, static check, translate, alpha convert. 
tests.ml       : code for parsing tests/manifest.txt and setting up testing. 
slang.ml       : main file, implementing the command-line for the interpreter and compiler 
interp_0.ml    : The "definitional" interpreter. 
                 slang_v2 will add four interpreters ... 


               
