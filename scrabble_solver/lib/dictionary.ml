type t = (char list, string) Hashtbl.t

let create_key (word : string) =
  List.init (String.length word) (fun n ->
      String.get word n |> Char.uppercase_ascii)
  |> List.sort Char.compare

let create_l d = Array.to_list d

let rec add_el d m =
  match d with
  | [] -> m
  | h :: t ->
      Hashtbl.add m (create_key h) h;
      add_el t m

let create_hash d =
  let lst = create_l d in
  let m = Hashtbl.create (List.length lst) in
  add_el lst m

let get_words (t : t) l =
  let key = create_key l in
  Hashtbl.find_all t key

let contains_word (d : t) (word : string) =
  let words = get_words d word in
  if List.length words = 0 then false
  else
    match List.find_opt (fun x -> x = word) words with
    | None -> false
    | _ -> true

let new_create_key (word : string) =
  let char_list = Word.string_to_char_list word in
  let array_key = Array.make 26 0 in
  List.iter
    (fun x ->
      let alpha_index = Char.code x - 65 in
      array_key.(alpha_index) <- array_key.(alpha_index) + 1)
    char_list;
  array_key
