
exception Error of string 

let complain s = raise (Error s) 



