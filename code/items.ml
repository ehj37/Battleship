open State
open Command

(** [buy_item st i] is the state after player 1 of [st] attempts to buy item 
    [i].*)
let buy_item st i = 
  match enough_money st.p_shop st.p1 i with
  | true -> 
    print_string ("Congratulations! You are now the proud owner of a new " ^ 
                  (string_of_item i) ^ ".\n" ^ "Enter a command.\n"); 
    {st with p1 = purchase_results st.p1 i}
  | exception ItemNotPresent -> raise ItemNotPresent
  | false -> raise NotEnoughMoney

(** [not_in_inv st i] is false if [i] is in player 1 of [st]'s inventory, and 
    true otherwise. *)
let not_in_inv st i = 
  match (remove_from_inv st.p1 i) with
  | exception ItemNotPresent -> true 
  | _ -> false

(** [shot_list_helper counter row acc] is a list all possible shots in row
    [row], as kept track of by [counter] and built upon [acc]. *)
let rec shot_list_helper counter row acc = 
  if counter = 10 then acc 
  else shot_list_helper (counter + 1) row ((counter+1, row)::acc)

(** [full_shot_list counter] is all possible shots on the grid, as kept track
    of by [counter]. *)
let rec full_shot_list counter = 
  if counter = 10 then [] 
  else (shot_list_helper 0 (counter + 1) []) @ (full_shot_list (counter + 1))

(** [not_in_shot_list st] is a list of all coordinates not in player 1's shot
    list in [st]. *)
let not_in_shot_list st = 
  let every_shot = full_shot_list 0 in 
  let player_shots = st.p1.shots in 
  List.filter (fun x -> List.mem x player_shots = false) every_shot

(** [potential_hits st] is a list of all possible shots that player 1 could take
    at player 2 in [st] that could result in a hit (given player 1's previous 
    shots). *)
let potential_hits st = 
  let potential_shots = not_in_shot_list st in 
  List.filter (fun x -> is_missed_shot x st.p2.ships = false) potential_shots

(** [potential_misses st] is a list of all possible shots that player 1 could 
    take at player 2 in [st] that could result in a miss (given player 1's 
    previous shots. *)
let potential_misses st = 
  let potential_shots = not_in_shot_list st in 
  List.filter (fun x -> is_missed_shot x st.p2.ships) potential_shots

(** [rnd_coord coord_list] is a random element of [coord_list].*)
let rnd_coord coord_list = 
  Random.self_init ();
  let rnd_index = Random.int (List.length coord_list) in 
  List.nth coord_list rnd_index

(** [string_of_coord coord] is a string representation of [coord]. *)
let string_of_coord coord = 
  match coord with 
  | (x,y) -> "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"

(** [shotsearcher3k st] is the state after item [ShotSearcher3000] is used in
    [st]. A coordinate is printed out as well.
    Requires: not all of player 2's ship coordinates in [st] have been fired 
    at. *)
let shotsearcher3k st = 
  Random.self_init ();
  let hit_list = potential_hits st in 
  let miss_list = potential_misses st in 
  if List.length miss_list = 0 then
    (print_string "Here's a secret: you can't miss at this point. \n"; st) 
  else let is_hit = Random.bool () in 
    if is_hit 
    then (print_string ("Shot Searcher 3000 shakes, and spits out the following 
    coordinate: " ^ (string_of_coord (rnd_coord hit_list)) ^ ".\n"); 
          {st with p1 = remove_from_inv st.p1 ShotSearcher3000}) 
    else (print_string ("Shot Searcher 3000 shakes, and spits out the following 
    coordinate: " ^ (string_of_coord (rnd_coord miss_list)) ^ ".\n"); 
          {st with p1 = remove_from_inv st.p1 ShotSearcher3000}) 

(** [three_shots st listy counter] is the state resulting from the firing at 
    [counter] random coordinates in [listy], or, if [listy] has a length of less 
    than [counter], is the state resulting from firing at all coordinates in 
    [listy]. Requires: [listy] must be composed of coordinates that player 1 in 
    [st] has yet to fire at.  *)
let rec three_shots st listy counter = 
  match listy, counter with
  | _, 0 -> st
  | [], _ -> st 
  | listy, _ -> Random.self_init ();
    let rnd_index = Random.int (List.length listy) in 
    let x1,y1 = List.nth listy rnd_index in 
    print_string ("fire " ^ string_of_int x1 ^ " " ^ string_of_int y1 ^ "\n");
    let new_listy = List.filter (fun x -> x <> (x1,y1)) listy in 
    let new_st = shot_results (x1,y1) st.p1 st.p2 st in
    three_shots new_st new_listy (counter - 1) 

(** [tripleshot st] is the state resulting from three unshot-upon coordinates
    in [st] being fired upon. If there are less than three such coordinates, 
    however many such coordinates are fired at. [TripleShot] is removed from
    player 1's inventory. *)
let tripleshot st = 
  let triple_st = three_shots st (not_in_shot_list st) 3 in 
  {triple_st with p1 = remove_from_inv triple_st.p1 TripleShot}

(** [missfinder st] is the state with item [MissFinder] removed from player 1's 
    inventory in [st]. It also prints the coordinate of a miss. *)
let missfinder st = 
  let misses = potential_misses st in 
  Random.self_init ();
  let rnd_index = Random.int (List.length misses) in 
  let x,y = List.nth misses rnd_index in 
  print_string 
    ("The miss finder beeps a bunch and returns the following coordinate: "
     ^ "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ").\n");
  {st with p1 = remove_from_inv st.p1 MissFinder}

(** [try_shots shot_list st] is the state resulting from all valid shots in 
    [shot_list] being fired in st*)
let rec try_shots shot_list st = 
  match shot_list with 
  | [] -> st 
  | (x,y)::t -> 
    try (let new_st = shot_results (x,y) st.p1 st.p2 st in 
         print_string ("fire "^string_of_int x ^ " " ^ string_of_int y ^ "\n"); 
         try_shots t new_st) 
    with | OutOfBounds | DuplicateShot -> try_shots t st

(** [squareshot st] is the state resulting from a 2x2 square of coordinates 
    being shot at in [st], with the lower-left coordinate being specified by the
    player. The player gains no money from the shots. *)
let rec squareshot st = 
  print_string ("Fire at the lower-left hand coordinate of the square, and the " 
                ^ "Square Shot will do the rest.\n");
  match parse (read_line ()) with
  | Fire (x,y) -> 
    (try (let new_st = shot_results (x,y) st.p1 st.p2 st in 
          let after_st = try_shots [(x+1,y);(x+1,y+1);(x,y+1)] new_st in 
          let player = {(remove_from_inv after_st.p1 SquareShot) 
                        with player_money = st.p1.player_money} in 
          {after_st with p1 = player})
     with | OutOfBounds -> 
       print_string "Fire at an in bounds coordinate!\n"; 
       squareshot st
          | DuplicateShot -> 
            print_string "You already fired there! Fire somewhere else!\n"; 
            squareshot st)
  | exception _ | _ -> 
    print_string "Fire at a valid coordinate!\n"; 
    squareshot st


(** [mysteryitem st] is the state resulting from removing item [MysteryItem]
    from player 1's inventory in [st] and adding in a mystery item. *)
let mysteryitem st = 
  let rnd_item = random_item () in 
  print_string ("After a flash of blinding light, you find a new " ^ 
                (string_of_item rnd_item) ^ ".\n");
  let removed_mys = remove_from_inv st.p1 MysteryItem in 
  let added_rnd = add_to_inv rnd_item removed_mys in
  {st with p1 = added_rnd}

(** [autosinker900 st] is the state resulting from the player choosing a 
    coordinate to send the AS900 at in [st]. If the coorinate is that of an 
    unsunk ship, that ship is sunk. Otherwise, nothing happens. One 
    [AutoSinker900] is removed from player 1's inventory. *)
let rec autosinker900 st = 
  print_string ("Fire the AS900 at a coordinate.\n");
  match parse (read_line ()) with
  | Fire coord -> 
    if is_sunk_shot coord st.p2.ships || is_missed_shot coord st.p2.ships 
    then (print_string "Nothing happens.\n"; 
          {st with p1 = remove_from_inv st.p1 AutoSinker900})
    else (print_string "Good job, you sunk a ship!\n"; sink_ship coord st)
  | Description i -> 
    print_string (item_description i); 
    autosinker900 st
  | exception _ | _ -> 
    print_string "Invalid command! Fire the autosinker somewhere. \n"; 
    autosinker900 st

(** [moneybag st] is the state from player 1 in [st] using [MoneyBag]. The 
    player gains a certain amount of money, and one [MoneyBag] is removed from 
    the player's inventory. *)
let moneybag st = 
  Random.self_init ();
  let payouts = [0;4;5;6;10] in
  let rnd_index = Random.int (List.length payouts) in 
  let payout = List.nth payouts rnd_index in 
  let payout_string = 
    (if payout = 0 then "absolutely nothing. Better luck next time.\n" 
     else if payout = 10 then "10 coins! Nice!\n" 
     else (string_of_int payout ^ " coins. Not bad.\n")) in
  print_string ("You peer into the bag and find " ^ payout_string);
  let added_money = {(st.p1) with player_money = st.p1.player_money + payout} in
  {st with p1 = remove_from_inv added_money MoneyBag}  

(** [inventory_swapper st] is the state resulting from [InventorySwapper] being
    removed from player 1's inventory in [st] before player 1 and 2's 
    inventories are swapped.*)
let inventory_swapper st = 
  let rem_item = remove_from_inv st.p1 InventorySwapper in 
  let new_p1 = {st.p1 with inventory = st.p2.inventory} in 
  let new_p2 = {st.p2 with inventory = rem_item.inventory} in
  print_string "You have swapped your inventory with the other player.\n";
  {st with p1 = new_p1; p2 = new_p2}



(** [use_item st i] is [st] after item [i] is used.
    Raises: [ItemNotPresent] if [i] isn't in player 1's inventory. *)
let use_item st i =
  if not_in_inv st i then raise ItemNotPresent
  else match i with 
    | TripleShot -> tripleshot st
    | SquareShot -> squareshot st
    | ShotSearcher3000 -> shotsearcher3k st
    | MysteryItem -> mysteryitem st
    | MissFinder -> missfinder st
    | AutoSinker900 -> autosinker900 st
    | InventorySwapper -> inventory_swapper st
    | MoneyBag -> moneybag st