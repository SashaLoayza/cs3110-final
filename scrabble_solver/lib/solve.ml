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

(** An array of all combinations of a given list of characters. The output array
    will have duplicate combinations in the case that the input array has
    duplicates.*)
let combinations char_list =
  let char_array = Array.of_list char_list in
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

(** [word_list dictionary char_list] is a list of valid words that can be made
    out of the letters in [char_list], using dictionary as the set of valid
    words.*)
let word_list dictionary char_list =
  let acc_unique_words (prev, acc) comb =
    if prev = comb then (comb, acc)
    else
      let new_words = Dictionary.get_words_key dictionary comb in
      let acc' = acc @ new_words in
      (comb, acc')
  in
  let acc = ([], []) in
  (* (prev_combination, accumulated_words) *)
  let combs = combinations char_list in
  let last, acc = Array.fold_left acc_unique_words acc combs in
  acc
