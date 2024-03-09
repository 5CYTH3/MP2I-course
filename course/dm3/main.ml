let exemple =
    [ "sire"; "site"; "ski"; "sac"; "dodos"; "dodu"; "dole"; "de"; "si"; "do" ]
;;

type trie =
    | V 
    | N of char * trie * trie
;;

type mot = char list;;

(**************)
(* Exercice 1 *)
(**************)

(* Question 1 *)
let rec est_bien_forme t =
    match t with
    | V -> true
    | N ('$', g, d) -> g = V && est_bien_forme d
    | N (_, g, d) -> est_bien_forme g && est_bien_forme d
;;

(* Question 2 *)
let mot_of_string s = 
    let n = String.length s - 1 in
    (* aux : int -> mot -> mot 
     * Prend en entrée un compteur [k] et une liste [l]. * À chaque itération, si le caractère à l'indice `k` dans la string s n'est pas le caractère `$`, passe à la prochaine
     * itération en incrémentant le compteur et en ajoutant à la liste l'élément à l'indice n-k avec n la taille de la string - 1
     * (car on ajoute les éléments en tête). Sinon, on renvoie la liste avec le dernier caractère ajouté en tête.
     *)
    let rec aux k l =
        if k = n then s.[n - k]::l
        else aux (k + 1) (s.[n - k]::l)
    in aux 0 ['$']
;;

let _ = assert (mot_of_string "yeah" = ['y'; 'e'; 'a'; 'h'; '$']);;

(* Question 3 *)
let rec afficher_mot (m: mot) = 
    match m with
    | [] | ['$'] -> print_endline ""
    | h::t -> print_char h; afficher_mot t
;;

let _ = "yeah$" |> mot_of_string |> afficher_mot

(**************)
(* Exercice 2 *)
(**************)

let trie_exemple =
  N
    ( 'd',
      N
        ( 'o',
          N
            ( '$',
              V,
              N
                ( 'l',
                  N ('e', N ('$', V, V), V),
                  N
                    ( 'd',
                      N
                        ( 'u',
                          N ('$', V, V),
                          N ('o', N ('s', N ('$', V, V), V), V) ),
                      V ) ) ),
          N ('e', N ('$', V, V), V) ),
      N
        ( 's',
          N
            ( 'i',
              N
                ( '$',
                  V,
                  N
                    ( 't',
                      N ('e', N ('$', V, V), V),
                      N ('r', N ('e', N ('$', V, V), V), V) ) ),
              N
                ( 'a',
                  N ('c', N ('$', V, V), V),
                  N ('k', N ('i', N ('$', V, V), V), V) ) ),
          V ) 
        )

(* Question 1 *)
let rec cardinal t =
    match t with
    | V -> 0
    | N ('$', V, d) -> 1 + cardinal d
    | N (_, g, d) -> cardinal g + cardinal d 
;;

let _ = assert (cardinal trie_exemple = 10);;

(* Question 2 *)
let recherche t m =
    (* [rev_m] représente le mot m à l'envers. Ce constant est utile
     * lors de la comparaison ligne 127, sachant que les éléments sont 
     * scannés de haut en bas, mais sont ajoutés en tête de liste.
     *)
let rev_m = List.rev m in
(* are_eq : mot -> mot -> bool
 * Vérifie que les mots [m1] et [m2] sont identiques.
     *)
let rec are_eq m1 m2 =
    match (m1, m2) with
        | [], [] -> true
        | _, [] | [], _ -> false
        | (h::t, h'::t') -> h = h' && are_eq t t'
in 
    (* sub_recherche : trie -> mot -> bool
     * Scanne les éléments du trie [t] de haut en bas, et ajoute dans un accumulateur 
     * représentant le mot en construction [m'] le caractère du noeud courant s'il n'est pas '$'.
     * Si c'est le cas, compare [m'] et [rev_m]. 
     *)
let rec sub_recherche t m' =
    match t with
        | V -> false
        | N ('$', V, d) -> are_eq rev_m ('$'::m') || sub_recherche d m'
        | N (c, g, d) -> sub_recherche g (c::m') || sub_recherche d m'
in
    sub_recherche t []
;;

let _ = assert (recherche trie_exemple ['s'; 'i'; 't'; 'e'; '$'] = true)

(* Question 3 *)
let rec insere t m = 
    match (t, m) with
    | _, [] -> t
    | V, h::t -> N (h, insere V t, V)
    | N (c, g, d), h::t ->
            if c = h then N (c, insere g t, d)
            else N (c, g, insere d m)
;;

let _ = assert (insere trie_exemple ['s'; 'i'; 't'; 'e'; '$'] = trie_exemple);;

(* Question 4 *)
let trie_of_list l =
    (* aux : string list -> trie -> trie 
     * Fonction auxiliaire permettant la récursivité terminale en utilisant [tr] comme accumulateur.
     * À chaque appel récursif, passe dans [tr] le [tr] courant auquel on a inséré le mot obtenu par la string courante.
     *)
    let rec aux l tr =
        match l with
        | [] -> tr
        | h::t -> aux t (insere tr (mot_of_string h))
    in aux l V
;;

let _ = assert (true = true);;

(* Question 5 *)
let rec longueur_max t =
    match t with
    | V -> -1
    | N (c, g, d) -> max (1 + longueur_max g) (longueur_max d)
;;

let _ = assert (longueur_max trie_exemple = 5);;

(* Question 6 *)
let compte_mots_longs t n =
    (* aux : trie -> int -> int 
     * utilise [k] comme accumulateur pour stocker la taille des mots scannés.
     *)
    let rec aux t k =
        match t with
        | V -> 0 
        | N ('$', V, d) -> (if k >= n then 1 else 0) + aux d k 
        | N (_, g, d) -> aux g (k + 1) + (aux d k)
in aux t 0
;;

let _ = assert (compte_mots_longs trie_exemple 4 = 5)

(* Question 7 *)
let iter_trie f t = 
    (* aux : trie -> mot -> unit 
     * Utilise [l] comme acumulateur pour stocker les mots scannés.
     *)
    let rec aux t l =
        match t with
        | V -> ()
        | N ('$', V, d) -> f l; aux d l
        | N (c, g, d) -> aux g (l @ [c]); aux d l
    in aux t []
;;

(* Question 8 *)
let affiche_mots t = iter_trie (afficher_mot) t;; 

let list_of_trie t =
    let l = ref [] in
    iter_trie (fun x -> l := x::!l) t;
    List.rev !l
;;

(* Question 9 *)
let tableau_occurences s = 
    let arr = Array.make 26 0 in 
    let rec aux m =
        match m with
        | [] | ['$'] -> ()
        | h::t -> arr.(int_of_char h - 97) <- arr.(int_of_char h - 97) + 1; aux t 
    in aux (mot_of_string s);
    arr
;;

let _ = assert (tableau_occurences "data" = [|2; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0|])

(**************)
(* Exercice 3 *)
(**************)

(* Question 1 *)
let cat_first_line s =
    let f = open_in s in
    f |> input_line |> print_string;
    close_in f
;;

let _ = cat_first_line "mots_petit.txt";;

(* Question 2 *)
let cat_first_100_lines s = 
    let f = open_in s in
    for k = 0 to 99 do
        try
            f |> input_line |> print_string;
            print_newline ();
        with End_of_file -> print_endline "Fichier terminé !"
    done;
    close_in f
;;

(*
let _ = cat_first_100_lines "mots_petit.txt";;
*)

(* Question 3 *)
let cat s =
    let f = open_in s in
    (* aux : in_channel -> unit
     * [fx] représente en quelque sorte la première ligne et [aux] se charge d'appliquer print_string sur cette ligne.
     * On effectue cette opération récursivement sur l'ensemble du fichier.
     *)
    let rec aux fx = 
        try
            fx |> input_line |> print_string;
            print_newline ();
            aux fx
        with End_of_file -> close_in f
    in aux f
;;

(*
let _ = cat "mots_petit.txt"
*)

(* A commenter *)
(* Question 4 *)
let trie_of_file s =
    let f = open_in s in
    (* aux : in_channel -> trie -> trie
     * On insère récursivement chaque lignes [fx] du fichier dans le trie [t]
     *)
    let rec aux fx t = 
        try
            fx |> input_line |> mot_of_string |> insere t |> aux fx
        with End_of_file -> close_in f; t
    in aux f V
;;

(*
let _ = print_int @@ cardinal (trie_of_file "mots_petit.txt");;
let _ = print_int @@ cardinal (trie_of_file "mots_gros.txt");; 
*)

(**************)
(* Exercice 4 *)
(**************)

let cinq_cent_mots = trie_of_file "mots_petit.txt";;
let ods6_lowercase = trie_of_file "mots_gros.txt";;

(* A priori, ça fonctionne, juste faire les tests propre et commenter aux*)
(* Question 1 *)
let sous_mots t s =
    let occ = tableau_occurences s in
    (* aux : trie -> mot -> unit
     * Parcours l'arbre et vérifie si le caractère du noeud courant est présent dans le mot [s] via sa table d'occurences.
     * Si c'est le cas, on parcours à gauche en stockant le caractère dans la liste [l], et on décrémente la valeur du caractère
     * dans la table d'occurences pour suivre le compte des utilisations. 
     * Sinon, on parcours à droite. Dans le cas d'un noeud contenant '$', on affiche le mot (reverse car on ajoute en tête
     * à chaque itération.
     *)
    let rec aux t l =
        match t with
        | V -> () 
        | N ('$', V, d) -> afficher_mot (List.rev l); aux d l
        | N (c, g, d) -> 
                if occ.(int_of_char c - 97) > 0 then begin 
                    aux d l; 
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) - 1;
                    aux g (c::l);
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) + 1
                end else
                    aux d l
    in aux t []
;;

(* let _ = sous_mots cinq_cent_mots "bonjour";; *)

(* Question 2 *)
let afficher_anagrammes t s =
    let occ = tableau_occurences s in
    let empty_arr = Array.make 26 0 in
    (* aux : trie -> mot -> unit
     * Même principe que précédemment sauf que on vérifie ici que TOUTES les lettres du tableau d'occurences sont utilisées.\
     * (C'est à dire on vérifie à chaque parcours de *mot* que le tableau d'occurences est vide.)
     *)
    let rec aux t l =
        match t with
        | V -> ()
        | N ('$', V, d) -> if occ = empty_arr then afficher_mot (List.rev l) else aux d l
        | N (c, g, d) -> 
                if occ.(int_of_char c - 97) > 0 then begin 
                    aux d l; 
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) - 1;
                    aux g (c::l);
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) + 1
                end else
                    aux d l
    in aux t []
;;

let _ = afficher_anagrammes cinq_cent_mots "avoir";;

(* Question 3 *)
let filtrer_sous_mots t s =
    let occ = tableau_occurences s in
    let trie = ref V in
    (* aux : trie -> mot -> unit
     * Même principe que la question 1 sauf que l'on n'affiche pas simplement le mot à 
     * chaque parcours mais on l'ajoute dans un nouveau trie [trie].
     *)
    let rec aux t l =
        match t with
        | V -> () 
        | N ('$', V, d) -> trie := insere !trie (List.rev ('$'::l)); aux d l
        | N (c, g, d) -> 
                if occ.(int_of_char c - 97) > 0 then begin 
                    aux d l; 
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) - 1;
                    aux g (c::l);
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) + 1
                end else
                    aux d l
    in aux t [];
    !trie
;;

let _ = print_int @@ cardinal @@ filtrer_sous_mots ods6_lowercase "banane";;

let filtrer_anagrammes t s =
    let occ = tableau_occurences s in
    let empty_arr = Array.make 26 0 in
    let trie = ref V in
    (* aux : trie -> mot -> unit 
     * Ici on combine les méthodes de la fonction précédente et de [afficher_anagrammes]
     *)
    let rec aux t l =
        match t with
        | V -> ()
        | N ('$', V, d) -> if occ = empty_arr then trie := insere !trie (List.rev ('$'::l)) else aux d l
        | N (c, g, d) -> 
                if occ.(int_of_char c - 97) > 0 then begin 
                    aux d l; 
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) - 1;
                    aux g (c::l);
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) + 1
                end else
                    aux d l
    in aux t [];
    !trie
;;

let _ = print_int @@ cardinal @@ filtrer_anagrammes ods6_lowercase "inserat";;

(* Question 4 *)
let filtrer_sur_mots t s =
    let occ = tableau_occurences s in
    let trie = ref V in
    let empty_arr = Array.make 26 0 in
    let rec aux t l =
        match t with
        | V -> ()
        | N ('$', V, d) -> if occ = empty_arr then trie := insere !trie (List.rev ('$'::l)); aux d l
        | N (c, g, d) -> 
                if occ.(int_of_char c - 97) > 0 then begin
                    aux d l; 
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) - 1;
                    aux g (c::l);
                    occ.(int_of_char c - 97) <- occ.(int_of_char c - 97) + 1
                end else (aux d l; aux g (c::l))
    in aux t [];
    !trie
;;

let _ = print_int @@ cardinal @@ filtrer_sur_mots ods6_lowercase "mpdeuxi";;

(**************)
(* Exercice 5 *)
(**************)

(* Question 1 *)
(* On a déjà l'insertion, la création d'un ensemble vide, le cardinal, la recherche. Il nous manque est_vide, element et enleve *)
(* Cependant je ne vois pas comment programmer `element` sachant qu'il n'y a aucune contraite particulière sur l'élément en question. *)
(* Je pourrais parcourir similairement l'arbre à iter_trie et renvoyer le premier élément ? Manque de temps *)

let est_vide t = t = V;;

(* Bon j'ai un peu de mal à réaliser cette fonction mais l'idée était de lorsque qu'un caractère est reconnu comme 
 * appartenant au mot à supprimer, on "rentre dans un état" de suppression de la partie de l'arbre que l'on parcours.
 * Tant que les caractères parcourus appartiennent au mot, on reste dans cet état mais si ce n'est pas le cas, on en sort
 * et on préserve donc l'état de l'arbre (grosse ressemblance avec un automate). 
 * Enfin rien de concluant.
 *)
let enleve t s =
    assert (recherche t (mot_of_string s) = true);
    let res = ref [] in
    let rec aux t m =
        match t with
        | V -> V
        | N ('$', V, d) -> if m = [] then (res := m; d) else  N ('$', V, aux d m)
        | N (c, g, d) -> 
            match m with
            | [] -> failwith "Cannot parse an empty word..."
            | h::tl -> if h = c then aux g tl else N (c, g, aux d m)
    in aux t [];
;;

(* let _ = assert (recherche (enleve trie_exemple ['s'; 'i'; 't'; 'e'; '$']) ['s'; 'i'; 't'; 'e'; '$'] = false) *)
