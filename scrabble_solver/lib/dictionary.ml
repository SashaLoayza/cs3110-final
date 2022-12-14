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

let get_words (t : t) s =
  let key = create_key s in
  Hashtbl.find_all t key

let get_words_key t key = Hashtbl.find_all t key

let contains_word (d : t) (word : string) =
  let words = get_words d word in
  if List.length words = 0 then false
  else
    match List.find_opt (fun x -> x = word) words with
    | None -> false
    | _ -> true
