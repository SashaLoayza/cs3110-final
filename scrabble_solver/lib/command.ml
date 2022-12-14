type t =
  | BoardAddWord of Word.t (*Add a word/succession of tiles to the board*)
  | BoardClear of Board.t (*Clear the board*)
  | HandMake of Letter.t list
    (* Clear the current hand, if any, and initialize a new hand of up to 7
       letters. *)
  | View (*Views the board*)
  | Undo (* Undo the last command*)
  | PERM
  | Solve of int * int (* Run the solver engine and print the top solutions.*)
  | Exit (* Exit the program*)
  | Empty (* Nothing *)

exception EmptyCommand
(**[parse_cmd cmd_string] is a list of strings, where the first string is the
   command passed in, and all subsequent strings are the arguments of the
   command (if any are given).Raises: an EmptyCommand exception, when the
   resulting list is empty. *)

let parse_cmd cmd_string : string list =
  let result =
    List.filter (fun x -> x <> " ") (String.split_on_char ' ' cmd_string)
  in
  match result with
  | [] -> raise EmptyCommand
  | _ -> result

let cmd_of_string s =
  let ic = parse_cmd s in
  match List.hd ic with
  | "ADD" ->
      if List.length ic < 5 then raise (Failure "Please enter all args for add")
      else
        let word = List.nth ic 1 in
        let row = int_of_string (List.nth ic 2) in
        let col = int_of_string (List.nth ic 3) in
        let dir = List.nth ic 4 in
        let direction =
          match dir with
          | "RIGHT" -> Word.Right
          | "DOWN" -> Word.Down
          | _ -> raise (Failure "Incorrect direction")
        in
        let pos = (row, col) in
        let letter_list = Letter.make_let_opt_list word in
        BoardAddWord
          {
            pos;
            direction;
            length = List.length letter_list;
            letter_list = List.rev letter_list;
          }
  | "VIEW" -> View
  | "HELP-SETUP" -> failwith "help"
  | "SOLVE" ->
      if List.length ic < 3 then raise (Failure "Please enter all args for add")
      else Solve (int_of_string (List.nth ic 1), int_of_string (List.nth ic 2))
  | "PERM" -> PERM
  | _ -> raise (Failure "Please enter something valid (add/view/help-setup)")
