type direction =
  | Right
  | Down

type t = {
  pos : int * int;
  direction : direction;
  length : int;
  letter_list : Letter.t option list;
}
