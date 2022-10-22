type t = char list

(** [from_char_list char_list] returns char_list, which is a valid value of type
    t in the current implementation. t. Requires: char_list is *)
let from_char_list (char_list : char list) : t = char_list

let letter_list (t : t) = List.map Letter.from_input t
let to_string_list t = List.map (String.make 1) t

(**[factorial n] is n!. Requires: n>1.*)
let factorial n =
  let rec factorial_tr n acc =
    if n = 0 then acc else factorial_tr (n - 1) (acc * n)
  in
  factorial_tr n 1

(**[perm_number n t] is the number of possible words made by the letters in
   [hand.t t]*)
let perm_number n t = factorial (List.length (letter_list t)) / factorial n

(** [unused_letters t perm] is the [char list] of the unused letters of *)
let unused_letters t perm =
  let result_array = Array.of_list t in
  (*result will be mutated to only include the unused letters of perm. The used
    letters will be changed to the char '0'.*)
  for n = 0 to Array.length result_array do
    if String.contains perm result_array.(n) then result_array.(n) <- '0'
    else ()
  done;
  List.filter (fun c -> c = '0') (Array.to_list result_array)

let create_perm perm_index perm t =
  let unused = unused_letters t perm in
  perm ^ String.make 1 (List.nth unused perm_index)

(**[permutations_n n t] is the list of all length n strings made by combining
   letters in [hand.t t ]. In the current implementation, there may be duplicate
   words if there are duplicate letters in [hand.t t]*)
let permutations_n n t (previous : string list) =
  let string_list = to_string_list t in
  let extra_letters = List.length string_list - n in
  match previous with
  | [] -> string_list
  | _ ->
      let result =
        Array.init (List.length previous) (fun prev_index ->
            Array.make extra_letters (List.nth previous prev_index))
      in
      Array.iteri
        (fun prev_index perm_array ->
          Array.iteri
            (fun perm_index perm ->
              perm_array.(perm_index) <- create_perm perm_index perm t)
            perm_array)
        result;
      result |> Array.to_list |> Array.concat |> Array.to_list

let permutations_helper (t : t) (current_result : string list array) n list_n =
  match n with
  | 0 -> ()
  | _ -> current_result.(n) <- permutations_n n t current_result.(n - 1)

let rec permutations t =
  let result = Array.make 7 [] in
  Array.iteri (permutations_helper t result) result;
  result |> Array.to_list |> List.concat

let word_list dictionary t =
  List.filter (fun perms -> Array.mem perms dictionary) (permutations t)
