let rec vectorize f l = match l with
| [] -> []
| h::t -> (f h)::(vectorize f t);;

let rec list_is_even = function
| []  -> []
| h::t -> (h mod 2 = 0)::(list_is_even t);;

let rec select f l = match l with
| [] -> []
| h::t -> if f h then h::(select f t) else select f t;;

let rec for_all p l = match l with
| [] -> failwith "doesnt work"
| h::[] -> p h
| h::t -> if p h then (for_all p t) else false;;
    
let rec exists p l = match l with
| [] -> failwith "nope"
| h::[] -> p h
| h::t -> if p h then true else exists p t;;

let rec compose l x = match l with
| [] -> x
| h::t -> h (compose t x);;

let rec apply l x = match l with
| [] -> x
| h::t -> apply t (h x);;

(* Q4:  *)
let rec is_ordered_unique l = match l with
| [] -> true
| _::[] -> true
| h::n::t -> if n = h then false else is_ordered_unique (n::t);;

let rec ordered_delete_duplicates l = match l with
| [] -> []
| h::[] -> [h]
| h::n::t -> if n = h then ordered_delete_duplicates (n::t) else h::(ordered_delete_duplicates (n::t));; 

(*let rec encode l = 
    let rec encode_aux ll k = 
*)

type expr = 
    | Int of string
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr;;

let is_literal e = match e with
| Int _ -> true
| _ -> false;;

let rec operator_count e = match e with
| Int _ -> 0
| Mul (e1, e2) | Sub (e1, e2) | Add (e1, e2) -> 1 + (operator_count e1 + operator_count e2);;

let rec eval e = match e with
| Int i -> int_of_string i
| Add (e1, e2) -> (eval e1) + (eval e2)
| Mul (e1, e2) -> (eval e1) * (eval e2)
| Sub (e1, e2) -> (eval e1) / (eval e2);;

let rec height e = match e with
| Int _ -> 1
| Mul (e1, e2) | Sub (e1, e2) | Add (e1, e2) -> if (height e1) > (height e2) then 1 + (height e1) else 1 + (height e2);;

let rec to_string e = match e with
| Int i -> i
| Mul (e1, e2) -> "(" ^ (to_string e1) ^ "*" ^ (to_string e2) ^ ")"
| Add (e1, e2) -> "(" ^ (to_string e1) ^ "+" ^ (to_string e2) ^ ")"
| Sub (e1, e2) -> "(" ^ (to_string e1) ^ "-" ^ (to_string e2) ^ ")";;

type color = Clubs | Spade | Heart | Diamond
type head = King | Queen | Jack
type value = Head of head | Number of int
type card = {color: color; value: value}

let duel c1 c2 = 
    let get_card_value c = match c.value with
    | Head h -> (match h with
        | King -> 13
        | Queen -> 12
        | Jack -> 11)
    | Number n -> n
    in
    if (get_card_value c1) = (get_card_value c2) then 0 else
        if (get_card_value c1) > (get_card_value c2) then -1 else 1;;

let rec game cl1 cl2 = if List.length cl1 = List.length cl2 then
    match cl1 with
    | [] -> 0
    | h1::t1 -> (match cl2 with
        | [] -> 0 | h2::t2 -> (duel h1 h2) + game t1 t2)
else failwith "Cannot play with two different sized card games"

(* Pretty unclear, kinda seems like we need to reuse the 'game' function but we also need to give back the cards in case of tie... *)
(* let rec war cl1 cl2 = if List.length cl1 = List.length cl2 then
        match cl1 with
        | [] -> 0
        | h1::t1 -> (match cl2 with
                | [] -> 0
                | h2::t2 -> (duel h1 h2) + war t1 t2)
else failwith "Cannot play with two different sized card games";; *)

type quotient = {
    den: int;
    num: int;
}

let ( #+ ) (x: quotient) (y: quotient) = { den = x.den * y.den; num = x.num * y.den + y.num * x.den };;
let ( #* ) (x: quotient) (y: quotient) = { den = x.den * y.den; num = x.num * y.num };;
let ( #/ ) (x: quotient) (y: quotient) = { den = x.num * y.den; num = y.num * x.den };; 
let ( #- ) (x: quotient) (y: quotient) = { den = x.den * y.den; num = x.num * y.den - y.num * x.den };;

type complex = {
    re: float;
    im: float;
}

let ( %+ ) (x: complex) (y: complex) = { re = x.re +. y.re; im = x.im +. y.im };;
let ( %- ) (x: complex) (y: complex) = { re = x.re -. y.re; im = x.im -. y.im };;
let ( %* ) (x: complex) (y: complex) = { re = (x.re *. y.re) -. (x.im *. y.im); im = (x.re *. y.im) +. (y.re *. x.im) }
(* How the fuck am I supposed to make a quotient of complex numbers *)
