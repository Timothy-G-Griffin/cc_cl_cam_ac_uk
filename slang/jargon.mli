
type code_index = int 
type stack_index = int 
type heap_index = int 
type static_distance = int 
type offset  = int 

type label = string 
type location = label * (code_index option) 

type status_code = 
  | Halted 
  | Running 
  | CodeIndexOutOfBound 
  | StackIndexOutOfBound 
  | HeapIndexOutOfBound
  | StackUnderflow 

type stack_item = 
  | STACK_INT of int 
  | STACK_BOOL of bool 
  | STACK_UNIT 
  | STACK_HI of heap_index    (* Pointer into Heap            *) 
  | STACK_RA of code_index    (* return address               *) 
  | STACK_FP of stack_index   (* Frame pointer                *) 

type heap_type =
    | HT_INL 
    | HT_INR 
    | HT_CLOSURE
    | HT_TUPLE

type heap_item = 
  | HEAP_INT of int 
  | HEAP_BOOL of bool 
  | HEAP_UNIT 
  | HEAP_HI of heap_index    (* Pointer into Heap            *) 
  | HEAP_CI of code_index    (* Code pointer for closures    *) 
  | HEAP_HEADER of int * heap_type (* int is number of items to follow *)       

type value_path = 
  | STACK_LOCATION of offset 
  | HEAP_LOCATION of offset 

type instruction = 
  | PUSH of stack_item    (* modified *) 
  | LOOKUP of value_path      (* modified *) 
  | UNARY of Ast.unary_oper 
  | OPER of Ast.oper 
  | ASSIGN 
  | SWAP
  | POP 
(*  | BIND of var            not needed *)
  | DEREF 
  | APPLY
  | RETURN
  | MK_TUPLE of int
  | MK_INL
  | MK_INR
  | MK_REF 
  | MK_CLOSURE of location * int (* modified *) 
  | TEST of location 
  | CASE of location
  | GOTO of location
  | LABEL of label 
  | HALT
  | INDEX of int


type vm_state = 
  {
    stack_bound : stack_index; 
    code_bound  : code_index; 
    heap_bound  : code_index; 
    stack       : stack_item array; 
    heap        : heap_item array; 
    code        : instruction array; 
    mutable sp : stack_index;  (* stack pointer *) 
    mutable fp : stack_index;  (* frame pointer *) 
    mutable cp : code_index;   (* code pointer  *) 
    mutable hp : heap_index;   (* next free     *) 
    mutable status : status_code; 
  } 

val new_label : unit -> string
			  
val step : vm_state -> vm_state 

val driver : int -> vm_state -> vm_state 

type listing = instruction list 

val comp : (Past.var * value_path) list ->
           Ast.expr -> instruction list * instruction list
			    
val compile : Ast.expr -> listing 

val run : listing -> vm_state

val interpret : Ast.expr -> vm_state

val string_of_listing : listing -> string 

val string_of_value : vm_state -> string 

