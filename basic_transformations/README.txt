
**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************

This directory contains Ocaml examples illustrating 
some basic transformations. 

tail.ml : Illustrates the introduction of tail recursion 
          and how tail-recursive function can be implemented
          by simple iteration, (no stack required). 

cps.ml : Illustrates "continuation passing style" --- 
         each function is transformed to take an additional 
         argument, called the continuation, representing 
         the rest of the computation.  The continuation 
         is a higher-order function. 

dfc.ml : Illustrates the process of "defunctionalisation". 
           That is, replacing higher-order functions with 
           data structures and "apply" functions. 
      
fibonacci_machine.ml : Illustrates using all of the above transformation 
         we derive a "Fibonacci Machine" from the 
         standard recursive definition for Fibonacci numbers. 
         This is done in several stages. 
         fib --> CPS 
             --> defunctionalise continuations,
                 see that resulting continuation datatype 
                 is just a list of tagged values. 
             --> replace continuations with lists (representing 
                 a stack) of tagged values 
             --> see that resulting code is really implementing 
                 a state-transition machine. 
                 Rewrite to get the Fibonacci Machine! 
          
expr_machine.ml : Similar to fibonacci_machine.ml, but in the 
                  end we take the extra step of deriving a "compiler".
         
        
      
