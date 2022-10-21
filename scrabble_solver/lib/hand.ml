type t = char list

(** [from_char_list char_list] returns char_list, which is a valid value of type
    t in the current implementation. t. Requires: char_list is *)
let from_char_list (char_list : char list) = char_list

let letter_list (t : t) = List.map Letter.from_input t
let permutations t = raise (Failure "Unimplemented: Hand.permutations")
let word_list dictionary t = List.filter (Array.mem dictionary) (permutations t)
