let exemple =
  [
    "sire";
    "site";
    "ski";
    "sac";
    "dodos";
    "dodu";
    "dole";
    "de";
    "si";
    "do";
  ]

type trie = V | N of char * trie * trie

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
          V ) )
