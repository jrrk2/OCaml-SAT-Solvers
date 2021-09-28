open Cnf
open Dpll
open Printf
open Scanf

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") > 0 with err -> false

(* generates a randomized cnf of length num_clauses in which each clause
 * has num_literals literals and the total number of distinct symbols is
 * bounded by num_syms *)
let generate_cnf (num_syms:int) (num_clauses:int) (num_literals:int) : cnf =
  (* utility method to generate clause of length num_literals *)
  let generate_clause () =
    let rec add_literal cnt cl =
      match cnt with
	  0 -> cl
	| _ -> let sym = "P" ^ string_of_int (Random.int num_syms) in
	       add_literal (cnt-1) ((sym, Random.bool ())::cl) in
    add_literal num_literals [] in
  let rec add_clause cnt cnf =
    match cnt with
	0 -> cnf
  | _ -> add_clause (cnt-1) (generate_clause ()::cnf) in
  add_clause num_clauses []

let solve cnf =
  if verbose then ( print_cnf cnf; print_newline () );
  let (b, ml) = dpll cnf in
  if b then print_endline "Solved" else print_endline "Unsolved";
  List.sort compare ml

let gen_solve_print (num_syms:int) (num_clauses:int) (num_literals:int) =
  let cnf = generate_cnf num_syms num_clauses num_literals in
  let ml = solve cnf in
  print_model ml

let rec skip_comment fd =
     let lin = String.trim (input_line fd) in
     if lin.[0] = 'c' then skip_comment fd else lin

let rec try_line fd =
     let lin = skip_comment fd in
     let lst = match String.split_on_char ' ' lin, String.split_on_char '\t' lin with 
       | _::[], _::[] -> failwith "delimeter not recognised" 
       | tl, _::[] -> tl
       | _::[], tl -> tl
       | _, _ -> failwith "multiple delimeters not recognised" in
     let lst = List.filter (fun x -> x <> "") lst in
     List.rev_map int_of_string lst

let rec get_lst fd = match try_line fd with
        | [] -> failwith "cnf: empty lines not allowed"
        | 0::tl -> List.rev tl
        | tl ->  List.rev tl @ get_lst fd

let systematic_cnf fil =
  let fd = open_in fil in
  let clause = ref [] in
  let m = ref 0 in
  let p,q = sscanf (skip_comment fd) "p cnf %d %d" (fun p q -> p,q) in
  let _ = try while true do
     let lst = get_lst fd in
     clause := List.map (fun itm -> let pos = itm > 0 in
         let abs = if pos then itm else -itm in
         m := max (!m) abs;
         ("P"^string_of_int abs), pos
       ) lst :: !clause;
  done with err -> close_in fd in
  close_in fd;
  let cnf = List.rev !clause in
  let len = List.length cnf in
  if p <> !m || q <> len then failwith (sprintf "Header %d,%d actual %d,%d\n" p q !m len);
  cnf
