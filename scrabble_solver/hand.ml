type t = char list

let from_char_list (char_list : char list) : t =
  List.sort Char.compare (List.map Char.uppercase_ascii char_list)

let char_list t = t
let size t = List.length (char_list t)
let letter_list (t : t) = List.map Letter.from_input (char_list t)
let to_string_list t = List.map (String.make 1) (char_list t)

let from_letter_list (lst : Letter.t list) : t =
  List.map (fun x -> Letter.char_value x) lst

(** [letter_n t n] is the nth letter of [Hand.t t]*)
let letter_n t n = List.nth (from_char_list t) n

(** duplicates_before t n] is the number of letters before the nth letter in [Hand.t t] that are equivalent to [letter_n t n]. Requires: n>=0. *)
let duplicates_before t n =
  let l = letter_n t n in
  let rec duplicates_helper t n acc =
    if n = 0 || letter_n t (n - 1) <> l then acc
    else duplicates_helper t (n - 1) (acc + 1)
  in
  duplicates_helper t n 0

(**[char_count c  s] is the number of occurrences of c in s.*)
let char_count c s =
  let rec char_count_helper (c : char) (s : string) (acc : int) =
    match s with
    | "" -> acc
    | s ->
        if not (String.contains s c) then acc
        else if s.[0] = c then
          char_count_helper c (String.sub s 1 (String.length s - 1)) (acc + 1)
        else char_count_helper c (String.sub s 1 (String.length s - 1)) acc
  in
  char_count_helper c s 0

(**[bit_n_of_word t s acc index] is the truth value of the [n]th bit in the
   bit-vector representation of string s. Requires: [s] is a string made up of
   only letters from [Hand.t t], that acc[0..n-1] is the correct partial
   bit-vector representation of word s, and that the value of [acc] at [index]
   is false. *)
let bit_n_of_word (t : t) (s : string) (acc : Bitv.t) (index : int) =
  let l = letter_n t index in
  if not (String.contains s l) then ()
    (*keep the bit false if the string does not contain the relevant letter*)
  else if char_count l s > duplicates_before t index then
    Bitv.set acc index true
    (*the letter is contained in s when there are more instances of the letter
      in the string than duplicates of the letter before position n in hand.*)
  else ()

let bit_word (s : string) (t : t) =
  let result = Bitv.create (size t) false in
  Bitv.iteri (fun index _ -> bit_n_of_word t s result index) result;
  result

let cons_if_unique acc elt = if List.mem elt acc then acc else elt :: acc
let unique a_list = List.fold_left cons_if_unique [] a_list

let cons_if_unused t acc index elt =
  if elt then acc (*elt is true so [letter_n t index] is used*)
  else letter_n t index :: acc

(** unused_letters t perm is a char list of the unique set of letters that are
    in [Hand.t t] but not in perm. Requires: All letters in perm are from
    [Hand.t t] and there are no more letters of any character than t has
    available. *)
let unused_letters (t : t) perm =
  bit_word perm t
  |> Bitv.foldi_left (fun acc index elt -> cons_if_unused t acc index elt) []
  |> unique

(**[create_perm perm_index perm unused] is the string with the perm_index^th
   unused letter added to the end of it.*)
let create_perm perm_index perm unused =
  perm ^ String.make 1 (List.nth unused perm_index)

(**[permutations_n t n] is the list of all length n strings made by combining
   letters in [Hand.t t ]. In the current implementation, there are no duplicate
   permutations returned. *)
let permutations_n t n (previous : string list) =
  let string_list = to_string_list t in
  match previous with
  | [] -> unique string_list
  | _ ->
      let result =
        Array.init (List.length previous) (fun prev_index ->
            let unused = unused_letters t (List.nth previous prev_index) in
            let current_perm = List.nth previous prev_index in
            Array.init (List.length unused) (fun perm_index ->
                create_perm perm_index current_perm unused))
        (*Initialize each perm list by adding each of the unused letters to the
          end of the current permutation*)
      in
      result |> Array.to_list |> Array.concat |> Array.to_list

let permutations_helper (t : t) (current_result : string list array) n list_n =
  match n with
  | 0 -> ()
  | _ -> current_result.(n) <- permutations_n t n current_result.(n - 1)

let permutations t =
  let result = Array.make (size t + 1) [] in
  Array.iteri (permutations_helper t result) result;
  result |> Array.to_list |> List.concat

(**[factorial n] is n!. Requires: 0<n<=20.*)
let factorial n =
  let rec factorial_tr n acc =
    if n <= 0 then acc else factorial_tr (n - 1) (acc * n)
  in
  factorial_tr n 1

(**[combination_number n] is the number of combinations of n objects (assuming
   they are all distinct). Requires: 0<n<=20.*)
let combination_number n =
  let rec combination_helper n k acc =
    if k <= 0 then acc
    else
      let acc' = acc + (factorial n / (factorial (n - k) * factorial k)) in
      combination_helper n (k - 1) acc'
  in
  combination_helper n n 0

(**[sum_combinations n] is the number of combinations of size 1 to n of a set of
   n distinct elements. Requires: 0<=n<=20*)
let sum_combinations n =
  let rec sum_helper n acc =
    if n <= 0 then acc else sum_helper (n - 1) (acc + combination_number n)
  in
  sum_helper n 0

let combinations s =
  List.fold_left
    (fun acc elt ->
      let new_key = Dictionary.create_key elt in
      if List.mem new_key acc then acc else new_key :: acc)
    [] (permutations s)

let new_combinations char_array =
  let index = ref 0 in
  let len = combination_number (Array.length char_array) in
  let result = Array.make len [] in
  Array.fast_sort Char.compare char_array;
  for i = Array.length char_array - 1 downto 0 do
    for j = 0 to !index - 1 do
      result.(!index) <- result.(!index) @ (char_array.(i) :: result.(j));
      print_endline (string_of_int !index);
      incr index
    done;
    print_endline (string_of_int !index);
    result.(!index) <- [ char_array.(i) ];
    incr index
  done;
  result

let word_list dictionary t =
  List.filter (fun perm -> Array.mem perm dictionary) (permutations t)
