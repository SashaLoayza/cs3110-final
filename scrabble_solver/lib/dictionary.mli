type t
(**A HashMap representation of all words in Dictionary.txt. The keys are
   bitvectors of letter combinations. The values are chains of the permutation
   of letters described by the bit vectors that create valid words as
   represented in dictionary.txt*)

val create_key : string -> char list
(*[create_key w] is the unique haschcode generated by the letters in word [w].
  Two words with the same letters will have the same [create_key w]. For example
  w= "tool" and w= "loot" have equivalent [create_key w]. *)

val new_create_key : string -> int array
(**[new_create_key word] is the int array [key] of length 26, where [key\[i\]]
   is the number of times letter [i] in the alphabet appears in [word].*)

val create_hash : string array -> t
(*[create_hash d] is the hashmap with (k,v) pairs being the bitvector and list
  of valid words respectively created from the dictionary input [d]*)

val get_words : t -> string -> string list
(*[get_words l] is all valid words of letter combinations defined by
  permutations of [l] *)

val contains_word : t -> string -> bool
(*[contains_word l] sees if word l is contained within the hashmap and returns
  true if it is and false otherwise*)
