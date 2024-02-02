type ide = string;;

type permission = Read | Write | Execute;;

(* ABSTRACT SYNTAX TREE*)
type expr = 
  EInt of int
  | EBool of bool
  | EString of string
  | EVar of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Mul of expr * expr
  | And of expr * expr 
  | Or of expr * expr
  | Equal of expr * expr
  | Greater of expr * expr
  | Lower of expr * expr
  | Fun of ide*expr*permission list (* Formal parameter * Function body * Permissions *)
  | Execute of expr*expr (* function name * actual parameter*)
;;

(* ENVIRONMENT *)
type 'v env = (string * 'v) list;;
let emptyenv = [];;

(* LANGUAGE TYPES *)
type value =
    Int of int
    | Bool of bool
    | String of string
    | Closure of (expr*ide*value env)
;;

let rec lookup (env: 'v env) x =
  match env with
  | [] -> failwith "Variable not found"
  | (ide, value)::r -> if x = ide then value else lookup r x;;

let bind (env: 'v env) (x, y) = (x, y)::env;;


(* FUNCTIONS *)
let rec checkPermission (p:permission) (p_list:permission list) : bool =
  match p_list with
  | [] -> false
  | per::others -> if (p = per) then true else checkPermission p others;;

let rec checkAllPermissions (all_perm: permission list) (p_list: permission list): bool =
  match all_perm with
  | [] -> true
  | p::l -> if (checkPermission p p_list) then checkAllPermissions l p_list else false
;;

let rec removePermission (p:permission) (p_list:permission list) : permission list =
  match p_list with
  | [] -> failwith("No permission to remove!")
  | perm::others -> if p = perm then others else perm :: (removePermission p others)
;;

let rec addPermission (p:permission) (p_list:permission list) : permission list =
  match p_list with
  | [] -> [p]
  | perm::others -> if p = perm then failwith ("Permission already existent!") else perm :: (addPermission p others)
;;


(* INTERPRETER *)

let rec eval (e:expr) (perm_list: permission list) (env: 'v env) =
  match e with
  | EBool b -> Bool b
  | EInt i -> Int i
  | EString s -> String s
  | EVar v -> lookup env v
  | Plus(op1, op2) -> let x1 = eval op1 perm_list env in
                        let x2 = eval op2 perm_list env in
                          begin match (x1, x2) with
                              | (Int e1, Int e2) -> Int(e1 + e2)
                              | (_, _) -> failwith("Error: operands are not integers")
                          end
  | Minus(op1, op2) -> let x1 = eval op1 perm_list env in
                          let x2 = eval op2 perm_list env in
                            begin match (x1, x2) with
                              | (Int e1, Int e2) -> Int(e1-e2)
                              | (_, _) -> failwith("Error: the operands are not integers")
                            end
  | Mul(op1, op2) -> let x1 = eval op1 perm_list env in
                      let x2 = eval op2 perm_list env in
                        begin match (x1, x2) with
                          | (Int e1, Int e2) -> Int(e1*e2)
                          | (_, _) -> failwith("Error: the operands are not integers")
                        end
  | And(op1, op2) -> let x1 = eval op1 perm_list env in
                      let x2 = eval op2 perm_list env in
                        begin match (x1, x2) with
                          | (Bool e1, Bool e2) -> Bool(e1 && e2)
                          | (_, _) -> failwith("Error: the operands are not booleans")
                        end
  | Or(op1, op2) -> let x1 = eval op1 perm_list env in
                      let x2 = eval op2 perm_list env in
                        begin match (x1, x2) with
                          | (Bool e1, Bool e2) -> Bool(e1 || e2)
                          | (_, _) -> failwith("Error: the operands are not booleans")
                        end
  | Equal(op1, op2) -> let x1 = eval op1 perm_list env in
                          let x2 = eval op2 perm_list env in
                            begin match (x1, x2) with
                              | (Bool e1, Bool e2) -> Bool(e1 = e2)
                              | (Int e1, Int e2) -> Bool(e1 = e2)
                              | (String e1, String e2) -> Bool (e1 = e2)
                              | (_, _) -> failwith("Unknown type of the variables")
                            end
  | Greater(op1, op2) -> let x1 = eval op1 perm_list env in
                          let x2 = eval op2 perm_list env in
                            begin match (x1, x2) with
                              | (Bool e1, Bool e2) -> Bool(e1 > e2)
                              | (Int e1, Int e2) -> Bool(e1 > e2)
                              | (String e1, String e2) -> Bool (e1 > e2)
                              | (_, _) -> failwith("Unknown type of the variables")
                            end
  | Lower(op1, op2) -> let x1 = eval op1 perm_list env in
                        let x2 = eval op2 perm_list env in
                          begin match (x1, x2) with
                            | (Bool e1, Bool e2) -> Bool(e1 < e2)
                            | (Int e1, Int e2) -> Bool(e1 < e2)
                            | (String e1, String e2) -> Bool (e1 < e2)
                            | (_, _) -> failwith("Unknown type of the variables")
                          end
  | Fun(fpar, fbody, flist) -> if (checkAllPermissions flist perm_list) then Closure(fbody, fpar, env)
                               else (failwith("Forbidden operation!"))
  | Execute(func, param) -> let closure = eval func perm_list env in
                              begin match closure with
                                | Closure (fbody, par, fenv) -> let p = eval param perm_list env in 
                                                                  let new_env = (par, p)::fenv in 
                                                                    eval fbody perm_list new_env
                                | _ -> failwith("ERROR: Not a closure!")
                              end;;

(* TESTBENCH *)

let myEnv : (value) env = [];;
let plist : permission list = [Read];; (*Global permission list*)

(* Example of a function that needs Read permission to be executed. In this case it would be allowed. *)
let f1 = Fun("x",Plus(EVar("x"), EInt(1)), [Read]) ;;
let exe1 = Execute(f1, EInt(5));;
eval exe1 plist myEnv;;

(* Enable write permission *)
let plist = addPermission Write plist;;

(* Try again a function that needs Write permission to be executed. In this case it would be allowed*)
let f3 = Fun("y",Mul(EVar("y"), EInt(3)), [Write]) ;;
let exe3 = Execute(f3, EInt(2));;
eval exe3 plist myEnv;;

(* Disable write permission *)
let plist = removePermission Write plist;;