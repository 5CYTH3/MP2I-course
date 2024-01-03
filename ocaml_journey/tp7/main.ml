(* EXERCICE 1 *)

type complex = {
    mutable re: float;
    mutable im: float;
}

let real_part (c: complex) = c.re;;
let imaginary_part (c: complex) = c.im;;

let conjugate c = c.im <- -. c.im;;

let reinit c = 
    c.im <- 0.0; 
    c.re <- 0.0
;;

let norm c = Float.sqrt (c.re ** 2.0 +. c.im ** 2.0);;

let incr_complex c = c.re <- c.re +. 1.0;;

let multiply_by c1 c2 = 
    c1.re <- c1.re *. c2.re +. c1.re *. c1.im;
    c1.im <- c1.im *. c2.re +. c2.im *. c1.im
;;

(* EXERCICE 2 *)

let f x = print_newline (print_int x);;

let g x y = Printf.printf "(%d, %f)\n" x y;; 

let rec iter f (l: 'a list) =  
    match l with
    | [] -> []
    | h::t -> f h; iter f t
;;

let show_int_list l = iter (Printf.printf "%d\n") l;;

(* EXERCICE 3 *)

let show_int_ref x = print_int !x;;

let swap_ref (x: 'a ref) (y: 'a ref) =
    let tmp = !x in
    x := !y;
    y := tmp
;;

let create_ref x = ref (x + 1);;

let n = 10;;

let _ = create_ref n;;

let incr_int_ref x = x := !x + 1;;

let incr_float_ref x = x := !x +. 1.0;;

let edit_first t z = 
    let (x, y) = !t in
    t := (z, y)
;; 

let edit_snd t z =
    let (x, y) = !t in
    t := (x, z)
;;

(* EXERCICE 4 *)

let int_sum n = 
    let x = ref 0 in
    for i = 0 to n do
        x := !x + i;
        print_newline (print_int !x)
    done;
    !x
;;

let rec euclid a b =
    let x = ref a in
    let y = ref b in
    while !y <> 0 do
        x := !y;
        y := !x mod !y
    done;
    !x
;;

let invert_imperative l =
    let x = ref l in
    let y = ref [] in
    while l <> [] do
        y := (List.hd !x)::!y;
        x := List.tl !x
    done;
    !y
;;

(* EXERCICE 5 *)

let oth_element a = a.(0);;

let occ_count a x =
    let rec aux a x i = 
        if i < Array.length a then
            if a.(i) = x then
                    1 + aux a x (i + 1 )
                else
                    aux a x (i + 1)
        else 0
    in aux a x 0
;;

let occ_count a x =
    let count = ref 0 in
    for k = 0 to Array.length a - 1 do
        if a.(k) = x then
            count := !count + 1
    done;
    !count
;;

let test_array = [|2; 3; 1; 4; 2; 5; 2; 5; 6; 1; 2|];; 

let rec bounds_sum t i j =
    if i < j then
        t.(i) + bounds_sum t (i + 1) j
    else 0
;;

let min_index a =
    let (min, index) = (ref a.(0), ref 0) in
    for k = 0 to Array.length a - 1 do
        if a.(k) < !min then begin
            min := a.(k);
            index := k
        end
    done;
    !index
;;

let swap_index a i j =
    let tmp = ref a.(i) in
    a.(i) <- a.(j);
    a.(j) <- !tmp
;;

let select_sort a =
    let n = Array.length a in
    for i = 0 to n - 2 do
        let min = ref i in
        for j = i + 1 to n - 1 do
            if a.(j) < a.(!min) then
                min := j
        done;
        if !min <> i then
            swap_index a i !min
    done
;;


(* Ici c' est des ints mais 'ca marche pareil avec des floats flemme de tester *)
let cumulative_sum a =
    let arr = Array.make (Array.length a) 0 in
    for k = 1 to Array.length a - 1 do
        arr.(k) <- arr.(k - 1) + a.(k)
    done;
    arr
;;

let _ = Array.iter (fun y -> Printf.printf " %d " y) (cumulative_sum test_array)

