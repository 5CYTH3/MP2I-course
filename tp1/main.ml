let norm (a: float) (b: float): float = Float.sqrt (a**2. +. b**2.);;

let euclidean_div a b = (a / b, a mod b);;

(* let power (x: int) (n: int): int = x ** n;; *)

let rec fib n = if n < 3 then 1 else (fib @@ n - 1) + (fib @@ n - 2);;

let rec efficient_fib n = 
        let rec aux n b a = 
                if n <= 0 then a else aux (n-1) (a+b) b 
        in aux n 1 0;;

let rec arith_geo_series (a: float) (b: float) n = 
        if n = 0 then 1. else a *. (arith_geo_series a b (n - 1)) +. b;;

let first_doubled = function
| [] -> []
| h::t -> (2*h)::t;;

let rec last_doubled l = match l with
| [] -> []
| x::[] -> (2*x)::[]
| h::t -> h::(last_doubled t);;

let rec list_sum = function
| [] -> 0
| h::t -> h + (list_sum t)

let rec length = function
| [] -> 0
| h::t -> 1 + (length t)

let list_average l = (list_sum l) / (length l)

let rec concat l1 l2 = match l1 with
| [] -> l2
| h::t -> h :: concat t l2


