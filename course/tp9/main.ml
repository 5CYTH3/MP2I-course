(**************
 * Exercice 1
 **************)

let print_q n b q l =
    if b then begin
        print_string "Question ";
        print_int n;
        print_string " : ";
        List.iter print_int (List.sort q l);
        print_newline ()
    end else ()
;;

(* Question 1 *)

(* Renvoie la liste triée dans l'ordre croissant *)
let _ = print_q 1 false (fun x y -> x - y) [7; 1; 3; 2; 0];; 

(* Renvoie la liste triée dans l'ordre décroissant *)
let _ = print_q 2 true (fun x y -> y - x) [7; 1; 3; 2; 0];; 

(* Question 2 *)
let tri_1 l =
    List.sort (fun x y -> abs x - abs y) l
;;

(* Question 3 *)
let tri_2 l =
    let sorting (x, u) (y, v) = 
        if x = y then
            abs(u) - abs(v)
        else abs(y) - abs(x)
    in
    List.sort sorting l
;;

(* Question 4 *)
let tri_4 l =
    List.sort (fun x y -> List.length x - List.length y) l
;;


