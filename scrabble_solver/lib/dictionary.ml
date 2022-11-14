type t = (int, string) Hashtbl.t

let hashcode w = raise (Failure "Unimplemented")
let create_l d = Array.to_list d

let rec add_el d m =
  match d with
  | [] -> m
  | h :: t ->
      Hashtbl.add m (hashcode h) h;
      add_el t m

let create_hash d =
  let lst = create_l d in
  let m = Hashtbl.create (List.length lst) in
  add_el lst m
