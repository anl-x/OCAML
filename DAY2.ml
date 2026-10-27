(* A basic OCAML Abstract Syntax Tree   *)
(* Question : https://fplaunchpad.org/ocaml_nptel/M06-L07-practice.html#problem-17-simplify *)

type ty = T_int | T_bool
type expr = Int of int
          | Bool of bool
          | Add of (expr *  expr)
          | Sub of (expr * expr)
          | Mul of (expr * expr)
          | Var of string
          | If of (expr * expr * expr)
          | Let_in of (expr * ty option * expr * expr)

let rec  pretty  = function 
          | (Var name) -> name
          | (Int x ) -> string_of_int x
          | (Bool x) -> string_of_bool x
          | (Add (exp1, exp2)) -> (pretty exp1) ^ " + " ^ (pretty exp2)
          | (Sub (exp1, exp2)) -> (pretty exp1) ^ " - " ^ (pretty exp2)
          | (Mul (exp1, exp2)) -> (pretty exp1) ^ " * " ^ (pretty exp2)
          | (If (expr1 , expr2 , expr3)) -> "if ( " ^ pretty expr1 ^ ") then " ^ pretty expr2 ^ " else " ^ pretty expr3
          | (Let_in (Var varname,type_a , expr1 , expr2)) -> "let  " ^ varname ^ (match type_a with | None -> "" | Some T_int -> " : int " | Some T_bool -> " : bool ") ^  " = " ^ pretty expr1 ^ " in  " ^ pretty expr2
          | _ -> ""
          
;; 
type value = VInt of int | VBool of bool

type env = (string * value ) list

let rec lookup name = function 
		  | [] -> None
		  | ((name',value) :: xs) -> if name = name' then (Some value) else (lookup name xs)

let eval expression =
  let rec eval' env  = function
  		  | (Int x) -> Some (VInt x)
  		  | (Bool x) -> Some (VBool x)
          | (Var name) -> (lookup name env)
          | (Add (exp1 , exp2)) -> (match (eval' env exp1 , eval' env exp2) with
          										| (Some VInt x, Some VInt y) -> (Some (VInt (x+y)))
          										| _ -> None)

          | (Sub (exp1 , exp2)) -> (match (eval' env exp1 , eval' env exp2) with
          										| (Some VInt x, Some VInt y) -> (Some (VInt (x-y)))
          										| _ -> None)

          | (Mul (exp1 , exp2)) -> (match (eval' env exp1 , eval' env exp2) with
          										| (Some VInt x, Some VInt y) -> (Some (VInt (x*y)))
          										| _ -> None)
          | (If (exp1, exp2 , exp3)) -> (match (eval' env exp1) with | Some (VBool true) -> eval' env exp2 | Some (VBool false) -> eval' env exp3 | _ -> None)
          | Let_in (Var varname, _ , exp1 , exp2) -> (match (eval' env exp1) with | Some value -> eval' ((varname,value) :: env ) exp2 | None -> None)

          | _ -> None
       in (eval' [] expression)

let expression = Let_in (Var "x" , Some T_int , Int 1, If (Bool true , Add ( Var "x"  , Int 3) , Int 4))
;;
print_endline @@ pretty expression
;;
print_endline @@ (match (eval expression) with None -> "bug / error "  | Some (VInt x) -> string_of_int x | Some (VBool x) -> string_of_bool x)

;; 

let rec constant_fold = function
    | Add (exp1, exp2)  -> (match (constant_fold exp1, constant_fold exp2) with
                                     | (Int x , Int y) -> (Int (x+y))
                                     | ( exp1' , exp2' ) -> Add ( exp1' , exp2' ))
    | Sub (exp1, exp2)  -> (match (constant_fold exp1, constant_fold exp2) with
                                     | (Int x , Int y) -> (Int (x-y))
                                     | ( exp1' , exp2' ) -> Sub ( exp1' , exp2' ))

    | Mul (exp1, exp2)  -> (match (constant_fold exp1, constant_fold exp2) with
                                     | (Int x , Int y) -> (Int (x*y))
                                     | ( exp1' , exp2' ) -> Mul ( exp1' , exp2' ))

    | (Int _ | Var _) as expr -> expr
    | _ -> failwith "not applicable"

let rec simplify = function
    | Add (exp, Int 0) | Add (Int 0, exp)  -> exp
    | Add (exp1, exp2)  -> (match (simplify exp1, simplify exp2) with
                                     | (Int 0, exp) |  (exp, Int 0) -> exp
                                     | (Int x , Int y) -> (Int (x+y))
                                     | ( exp1' , exp2' ) -> Add ( exp1' , exp2' ))
    | Sub (exp1, exp2)  -> (match (simplify exp1, simplify exp2) with
                                     | (exp, Int 0) -> Int 0
                                     | (Int x , Int y) -> (Int (x-y))
                                     | ( exp1' , exp2' ) -> Sub ( exp1' , exp2' ))

    | Mul (exp1, exp2)  -> (match (simplify exp1, simplify exp2) with
                                     | (Int 0, exp) |  (exp, Int 0) -> Int 0
                                     | (exp , Int 1) |  (Int 1 , exp) -> exp
                                     | (Int x , Int y) -> (Int (x*y))
                                     | ( exp1' , exp2' ) -> Mul ( exp1' , exp2' ))

    | (Int _ | Var _) as expr -> expr
    | _ -> failwith "not applicable"

                            
