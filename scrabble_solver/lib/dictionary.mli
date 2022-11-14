type t
(**A HashMap representation of all words in Dictionary.txt. The keys are
   bitvectors of letter combinations. The values are chains of the permutation
   of letters described by the bit vectors that create valid words as
   represented in dictionary.txt*)

(*val get_bitv : string -> Bitv.t*)
val hashcode : Bitv.t -> int
(*[hashcode] is the unique haschcode generated by the list of letters in word
  [w]. Two words with teh same letters will have the same [hashcode w]. For
  example w= "tool" and w= "loot" have equivalent [hashcode w]. *)

val create_hash : string array -> t
(*[create_hash d] is the hashmap with (k,v) pairs being the bitvector and list
  of valid words respectively created from the dictionary input [d]*)

val get_words : t -> int -> string list
(*[get_words l] is all valid words of letter combinations defined by
  permutations of [l] *)
