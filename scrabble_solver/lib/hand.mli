type t
(** A player's hand, with letters sorted in alphabetical order.*)

val from_char_list : char list -> t
(** [from_char_list char_list] is the representation of the players hand as a
    value of type [Hand.t]. It sorts the input char list so that t is sorted.*)

val char_list : t -> char list
(** [char_list char_list] is the representation of the players hand as a value
    of type [char list], where all chars are in uppercase ascii representation.*)

val size : t -> int
(**[size t] is the number of letters in [Hand.t t]*)
val letter_list : t -> Letter.t list
(** [letter_list t] is a [Letter.t list] of the letters in [t:hand]*)

val unique : char list -> char list
(** [unique chars] is the char list of the characters in chars with duplicates
    removed*)

val bit_word : string -> t -> Bitv.t
(**bit_hand is the (7-bit) bitwise [Bitv.t] representation of [string s] with
   respect to the initial ordering of [Hand.t t], which is maintained in
   [char_list t]. If there are duplicate letters in hand, then they are
   populated left to right in the bitwise representation of the integer.For
   example, if [char_list t] is ['a', 'b', 'b', 'd', 'e', 'x', 'y', ], then
   [bit_word "bad"] is the int 1101000. *)

val unused_letters : t -> string -> char list
(** [unused_letters t perm] is the [char list] of the unused letters of
    [Hand.t t] in string [perm].*)

val permutations : t -> string list
(** [permutations t] is the [string list] of all possible permutations of the
    letters in [Hand.t t]. In the current implementation, this includes
    duplicates if [t] has duplicate letters.*)

val word_list : string array -> t -> string list
(** [word_list dictionary t] is a list of valid words that can be made out of
    the seven letters in [t:hand], using dictionary as the set of valid words.*)
