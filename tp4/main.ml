(*1./*)
(*Q1*)
type coloration = 
    | Cyan
    | Magenta
    | Jaune
    | Melange of coloration * coloration
;;

let rouge = Melange(Magenta, Jaune);;
let orange = Melange(rouge, Jaune);;

(* Q2: this function translates any color into RGB values (CMJ here)*)

(*2./*)
(*Q3*)
type 'a linkedlist =
    | Nil
    | Cons of 'a * 'a linkedlist
;;

let liste = Cons(1, Cons(2, Cons(3, Nil)));;

let cons h t = Cons(h, t);;
(*Q4*)
cons 0 liste;;

(*Q5*)
let head l = match l with
| Nil -> failwith "There is no head in this list"
| Cons (a, _) when a = Nil -> failwith "This list has no head"
| Cons (a, _ ) -> a;;

let tail l = match l with
| Nil -> failwith "Cannot return the tail of an empty list"
| Cons(_, t) -> t;;

(*Q6*)
let rec length l = match l with
| Nil -> 0
| Cons(_, t) -> 1 + length t;;

(*Q7*)
let rec belongsto l x = match l with
| Nil -> false
| Cons(h, t) -> (h = x) || (belongsto t x);;

(*Q8*)
let rec delete l x = match l with
| Nil -> Nil
| Cons(h, t) -> if h = x then delete t x else Cons(h, delete t x);;

(*Q9*)
(*To be fair, I don't really know how to do that*)
let nth l n = match l with
| Nil -> n
| Cons(h, t) -> 0;;
