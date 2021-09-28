open Generator
open Cnf
open Dpll

let _ = print_model (solve (if Array.length Sys.argv > 1 then systematic_cnf Sys.argv.(1) else generate_cnf 4 3 3))
