open State

type coordinates = (int * int)

type location = (int * int * int * int)

type player_item = string list

type shopping_item = string

type command = 
  | Quit
  | Fire of coordinates
  | CheckShips
  | Place of location
  | Inventory
  | Money
  | Shop
  | Use of item
  | Buy of item
  | TwoPlayer
  | OnePlayer
  | Easy
  | Medium
  | Hard
  | Instructions
  | Description of item

exception Empty

exception Malformed

(*[helper_remove_white_space_between] removes white spaces betweens words 
  ensuring no weird spacing bugs in comand. Helper functon for str function **)
let rec helper_remove_white_space_between (str_lst: string list) 
    (result : string list)= 
  match str_lst with
  | [] -> result
  | h::t -> if h = "" then helper_remove_white_space_between t result else 
      helper_remove_white_space_between t (h :: result)

(** [valid_coordinates str_lst] is [str_lst] if [str_lst] is a list composed of
    two strings that represent integers. 
    Raises: [Malformed] if [str_lst] has a length not equal to two or has one or
    more elements that aren't string representations of integers.*)
let valid_coordinates str_lst = 
  if List.length str_lst <> 2 
  then raise Malformed 
  else 
    begin
      match List.map int_of_string str_lst with
      | exception (Failure _) -> raise Malformed
      | [x;y] -> (x,y)
      | _ -> failwith "Needed this pattern match to compile"
    end

(** [item_cmnd listy] is [item] if the strings in [listy] correspond to a 
    valid item [item].
    Raises: [Malformed] if the strings in [listy] don't correspond to a valid
    item.*)
let item_cmnd listy = 
  match listy with
  | ["triple";"shot"] -> TripleShot
  | ["square";"shot"] -> SquareShot
  | ["shot";"searcher";"3000"] | ["ss3000"] -> ShotSearcher3000
  | ["mystery";"item"] -> MysteryItem
  | ["auto";"sinker";"900"] | ["as900"] -> AutoSinker900
  | ["miss";"finder"] -> MissFinder
  | ["inventory";"swapper"] | ["inv swapper"] -> InventorySwapper
  | ["money";"bag"] -> MoneyBag
  | _ -> raise Malformed

(** [valid_placement c] is [c] if [c] are a valid set of four coordinate
  * ints *)
let valid_placement c = 
  if (List.length c <> 4) 
  then raise Malformed
  else 
    begin 
      match List.map int_of_string c with
      | exception (Failure _) -> raise Malformed
      | [x1; x2; y1; y2] -> (x1, x2, y1, y2)
      | _ -> failwith "Needed this pattern match to compile" 
    end 

let parse str = 
  let lowercase_str = String.lowercase_ascii str in
  match String.split_on_char (' ') (String.trim lowercase_str) with
  | ["quit"] -> Quit
  | ["checkships"] -> CheckShips
  | "fire" :: a -> (
      match valid_coordinates a with
      | exception Malformed -> raise Malformed
      | x -> Fire x)
  | "place" :: coordinates -> 
    begin 
      match valid_placement coordinates with 
      | exception Malformed -> raise Malformed
      | c -> Place c
    end
  | [""] -> raise Empty
  | ["inventory"] -> Inventory
  | ["money"] -> Money
  | ["shop"] -> Shop
  | ["one"; "player"] -> OnePlayer
  | ["two"; "player"] -> TwoPlayer
  | ["easy"] -> Easy
  | ["medium"] -> Medium
  | ["hard"] -> Hard 
  | "buy" :: a -> Buy (item_cmnd a)
  | "use" :: b -> Use (item_cmnd b)
  | ["instructions"] -> Instructions
  | "description" :: a -> Description (item_cmnd a)
  | _ -> raise Malformed