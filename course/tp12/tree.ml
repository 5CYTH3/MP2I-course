type 'a abr = V | N of 'a abr * 'a * 'a abr

let exemple_3 =
  N
    ( N (N (N (V, -6., V), -5.1, N (V, -4.8, V)), 1.2, N (V, 3.1, V)),
      3.5,
      N (N (V, 3.8, V), 5.1, N (V, 10.5, V)) )

