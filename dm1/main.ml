let rec puissance x n = if n = 0 then 1 else if n < 0 then failwith "non" else (puissance x (n - 1)) * x;;

let rec somme_n_puissance_k n k = if n = 0 then 0 else puissance n k + somme_n_puissance_k (n - 1) k;;

let rec binom k n = if k = n || k = 0 || n = 0 then 1 else binom (k - 1) (n - 1) + binom k (n - 1)
