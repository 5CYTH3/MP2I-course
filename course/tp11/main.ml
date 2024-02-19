type ('a, 'b) tree =
    | Leaf of 'a
    | Node of 'b * ('a, 'b) tree * ('a, 'b) tree
;;

let exemple1 = Node (12,
    Node(4, 
        Node(7, Leaf true, Leaf false),
        Node(14, Leaf true, Leaf true)
    ),
    Leaf false
)

let exemple2 = Node(4,
    Leaf 0.3,
    Node(1, 
        Node(8, Node(2, Leaf 2.5, Leaf 3.1), Leaf 4.1),
        Leaf 0.2
    )
)

(* Question 3 *)
let rec size t = 
    match t with
    | Leaf x -> 0
    | Node (e, l, r) -> 1 + size l + size r
;;

(* Question 4 *)
let rec last t = 
    match t with
    | Leaf x -> x
    | Node (e, l, r)  -> last r
;;

(**************)
(* EXERCICE 2 *)
(**************)

(* Question 2 *)
let rec show_prefix t =
    match t with
    | Leaf x -> print_float x
    | Node (e, l, r) -> begin
        print_int e;
        show_prefix l;
        show_prefix r
    end
;;

(* Question 3 *)
let rec show_infix t =
    match t with
    | Leaf x -> print_float x
    | Node (e, l, r) -> begin
        show_prefix l;
        print_int e;
        show_prefix r
    end
;;

let rec show_postfix t =
    match t with
    | Leaf x -> print_float x
    | Node (e, l, r) -> begin
        show_prefix l;
        show_prefix r;
        print_int e
    end
;;

(* Question 4 *)
(* A finir j'ai rien compris *)
let rec show_width t = 
    let q = Queue.create () in
    Queue.push t q;
    while not (Queue.is_empty q) do
        match Queue.pop t with
        | Leaf x -> print_float x
        | Node (e, l, r) -> print_int e
    done
;;

let rec label_width t = 
    let q, sol = Queue.create (), [] in
    Queue.push t q;
    let rec aux acc queue =
        if Queue.is_empty q then acc else
            let l = Queue.pop q in
            match l with
            | Leaf x -> aux ((F x)::acc) queue
            | Node (e, l, r) -> 
                    Queue.push l queue;
                    Queue.push r queue;
                    aux ((N e)::acc) queue
    in List.rev (aux [] q)
;;

(**************)
(* EXERCICE 3 *)
(**************)

type ('a, 'b) token = F of 'a | N of 'b;;

(* Question 1 *)
let rec naive_postfix t =
    match t with
    | Leaf x -> [F x]
    | Node (e, l, r) -> (naive_postfix l @ naive_postfix r) @ [N e]
;;

let _ = assert(naive_postfix exemple2 = [F 0.3; F 2.5; F 3.1; N 2; F 4.1; N 8; F 0.2; N 1; N 4])

(* Question 2 *)
let postfix t =
    let rec aux acc t =
        match t with
        | Leaf x -> (F x)::acc 
        | Node (e, l, r) -> aux (aux ((N e)::acc) r) l 
    in aux [] t
;;

let _ = assert (postfix exemple2 = [F 0.3; F 2.5; F 3.1; N 2; F 4.1; N 8; F 0.2; N 1; N 4])

(* Question 3 *)
let prefix t =
    let rec aux acc t =
        match t with
        | Leaf x -> (F x)::acc 
        | Node (e, l, r) -> aux (aux ((N e)::acc) l) r 
    in aux [] t
;;

let _ = assert (prefix exemple2 = [F 0.2; F 4.1; F 3.1; F 2.5; N 2; N 8; N 1; F 0.3; N 4])

let infix t =
    let rec aux acc t =
        match t with
        | Leaf x -> (F x)::acc 
        | Node (e, l, r) -> aux ((N e)::aux (acc) r) l 
    in aux [] t
;;

let _ = assert (infix exemple2 = [F 0.3; N 4; F 2.5; N 2; F 3.1; N 8; F 4.1; N 1; F 0.2])

(**************)
(* EXERCICE 4 *)
(**************)

(* Question 2 *)
(* Il est raisonnable d'utiliser une liste pour stocker les étiquettes lues au fur et à mesure des itérations car il suffit d'ajouter les éléments au fur et à mesure puis à reverse la liste à la fin.
   Il n'est pas raisonnable d'utiliser une liste pour stocker les arbres à traiter car les opérations pour réorganiser la liste sont coueuses (@).
 *)

(* Question 3 *)
let rec label_width t = 
    let q, sol = Queue.create (), [] in
    Queue.push t q;
    let rec aux acc queue =
        if Queue.is_empty q then acc else
            let l = Queue.pop q in
            match l with
            | Leaf x -> aux ((F x)::acc) queue
            | Node (e, l, r) -> 
                    Queue.push l queue;
                    Queue.push r queue;
                    aux ((N e)::acc) queue
    in List.rev (aux [] q)
;;

let _ = label_width exemple2;; 

(**************)
(* EXERCICE 5 *)
(**************)

(* Question 1 *)
(* Chiant à faire mais TODO *)

(* Question 2 *)
let rec read_label l t =
    match (l, t) with
    | ([], Leaf x) -> F x
    | ([], Node (e, l, r)) -> N e
    | (h::t, Leaf x) -> failwith "The given map doesn't match any address."
    | (h::t, Node(e, l, r)) -> 
            if h then 
                read_label t r 
            else read_label t l
;;

(* Question 3 *)
let rec increment l t =
    match (l, t) with
    | ([], Leaf x) -> F x
    | ([], Node (e, l, r)) -> N (e + 1)
    | (h::t, Leaf x) -> failwith "The given map doesn't match any address."
    | (h::t, Node(e, l, r)) -> 
            if h then 
                increment t r 
            else increment t l
;;

(* Question 4 *)
(* TODO (et toujours aussi chiant) *)
let affiche_addresse addr =
    List.iter (fun b -> print_string (if b then "0" else "1")) addr
;;

let show_addresses t addr = 
    match t with
    | Leaf x -> addr
    | Node (e, l, r) -> begin
    end
;;


