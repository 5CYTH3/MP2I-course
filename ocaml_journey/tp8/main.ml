(*****************
 * EXERCICE 5
 ******************)

type 'a queue_fonc = 'a list * 'a list;;

let empty_queue = ([], []);;

(* Question 1 *)
let is_empty f = 
    match f with
    | [], [] -> true
    | _, _ -> false
;;

let push f x =
    let l1, l2 = f in
    (x::l1, l2)
;;


let list_to_file l = [], l;;

(* Question 2 *)

(* let rec are_equal q r = 
    match (q, r) with
    | (a, b) when is_empty a && is_empty b -> true
    | (a, b) when is_empty a || is_empty b -> false
    | ((h1::t1, _), (h2::t2, _)) -> if h1 = h2 then are_equal (pop q) (pop r) else false
*)

(* Question 3 *)
let pop f = 
    match f with
    | l1, h2::t2 -> (h2, (l1, t2)) 
    | l1, [] ->
            match List.rev l1 with
            | [] -> failwith "empty queue"
            | t::q -> (t, ([], q))
;;

(* Question 6 *)
(* A FINIR MDRR *)

type 'a queue_imp = {
    start: 'a;
    endd: 'a;
}

let rec somme q =
    if is_empty q then 0
    else 
        let (x, r) = pop q in
        x + somme r
;;

let rec show_queue q =
    if is_empty q then () else
    let (x, r) = pop q in
    show_queue r;
    print_int x
;;

(* TODO *)
let queue_to_list q = q;;

(******************
 * EXERCICE 6
 ******************)

type 'a vector = {
    mutable data: 'a array;
    mutable length: int;
    default: 'a
}

(* Question 1 *)

let make x n =
    { data = Array.make x n; length = n; default = x; }
;;

let get v k = Array.get v.data k;;

let set v k x = Array.set v.data k x;;

let length v = v.length;;

(* Question 2 *)
let resize_naive v n =
    if n > length v then
        v.data <- Array.make v.default n;
        v.length <- n
;;

(* Question 4 *)
let resize v n =
    if n > length v then
        let len = 2. ** Float.log2(float_of_int n) in
        v.data <- Array.make v.default (int_of_float len);
        v.length <- (int_of_float len)
;;


(******************
 * EXERCICE 7
 ******************)

(* Question 1 : 51 *)
(* Question 2 : 7 9 - 2 * 3 + 6 * *)

type operator =
    | Plus
    | Minus
    | Mul
    | Div
;;

type postfix_expr =
    | Val of int
    | Op of operator
;;

(* Question 3 *)
let ex_1 = [Val 3; Val 14; Op Plus; Val 5; Val 2; Op Minus; Op Mul]
let ex_2 = [Val 7; Val 9; Op Minus; Val 2; Op Mul; Val 3; Op Plus; Val 6; Op Mul]

(* Question 4 *)
let op_app (o: operator): (int -> int -> int) =
    match o with
    | Mul -> ( * )
    | Div -> ( / )
    | Minus -> ( - )
    | Plus -> ( + )
;;

let eval_postfix l =
    let s = Stack.create () in 
    (*  *)
    let rec eval_expr lx =
        match lx with
            | [] -> Stack.pop s
            | h::t -> begin
                match h with
                | Val x -> Stack.push x s
                | Op o -> 
                    Stack.push ((op_app o) (Stack.pop s) (Stack.pop s)) s
            end;
            eval_expr t
    in
    match l with
    | [] -> failwith "Incorrect operation"
    | _ -> eval_expr l 

;;

(* Question 5 *)
type infix_expr = 
    | Val2 of int
    | LParen
    | RParen
    | Op2 of operator
;;

let display_infix_expr o =
    match o with
    | Val2 x -> print_int x
    | Op2 o -> begin
        match o with
        | Mul -> print_string "*"
        | Div -> print_string "/"
        | Plus -> print_string "+"
        | Minus -> print_string "-"
    end
    | LParen -> print_string "("
    | RParen -> print_string ")"
;;

let is_well_parenthesized e =
    let lparen_count = ref 0 in
    let rparen_count = ref 0 in
    begin match e with
    | Val2 x -> ()
    | Op2 o -> ()
    | LParen -> lparen_count := !lparen_count + 1
    | RParen -> rparen_count := !rparen_count + 1
    end;
    lparen_count = rparen_count
;;



(* Question 6 *)
let eval_infix l =
    let operand_s = Stack.create () in
    let operator_s = Stack.create () in
    let rec eval_expr lx =
        match lx with
        | [] -> operand_s
        | h::t -> begin
            match h with
            | Val2 x -> Stack.push (Val2 x) operand_s
            | LParen -> Stack.push LParen operator_s
            | Op2 o -> Stack.push (Op2 o) operator_s
            | RParen -> begin
                Stack.push (Stack.pop operator_s) operand_s;
                let _ = Stack.pop operator_s  in ()
            end 
        end;
        eval_expr t
    in
    match l with
    | [] -> failwith "Cannot process empty list"
    | _ -> eval_expr l
;;

let ex_i2 = [LParen; LParen; LParen; Val2 2; Op2 Mul; LParen; Val2 7; Op2 Minus; Val2 9; RParen; RParen; Op2 Plus; Val2 3; RParen; Op2 Mul; Val2 6; RParen]

let _ = Stack.iter display_infix_expr (eval_infix ex_i2)

