type t
(**A HashMap representation of all words in Dictionary.txt. The keys are
   bitvectors of letter combinations. The values are chains of the permutation
   of letters described by the bit vectors that create valid words as
   represented in dictionary.txt*)

val get_bitv : string -> Bitv.t
