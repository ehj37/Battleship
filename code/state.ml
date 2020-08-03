open Graphics

exception ItemNotPresent
exception NotEnoughMoney

(** Type of ships in Destroyer | Carrier | Submarine | Battleship | Cruiser *)
type ship = (int*int*bool) list

(* Shot Searcher 3000 *) 
type item = TripleShot | SquareShot | ShotSearcher3000 | MysteryItem
          | MissFinder | AutoSinker900 | InventorySwapper | MoneyBag

let non_default_items = [TripleShot;SquareShot;ShotSearcher3000;AutoSinker900;
                         InventorySwapper;MoneyBag]

let item_price i = 
  match i with
  | TripleShot -> 7
  | SquareShot -> 8
  | ShotSearcher3000 -> 15
  | MysteryItem -> 7
  | MissFinder -> 5
  | AutoSinker900 -> 13
  | InventorySwapper -> 10
  | MoneyBag -> 5

let string_of_item i = 
  match i with
  | TripleShot -> "Triple Shot"
  | SquareShot -> "Square Shot"
  | ShotSearcher3000 -> "Shot Searcher 3000"
  | MysteryItem -> "Mystery Item" 
  | MissFinder -> "Miss Finder"
  | AutoSinker900 -> "Auto Sinker 900"
  | InventorySwapper -> "Inventory Swapper"
  | MoneyBag -> "Money Bag"

let item_description i = 
  match i with
  | TripleShot -> "Three random coordinates will be fired at.\n"
  | SquareShot -> "Fires a 2x2 square, given a specified lower-left corner.\n"
  | ShotSearcher3000 -> "Returns a coordinate that has a 50% chance of being" ^
                        " a hit and a 50% chance of being a miss.\n"
  | MysteryItem -> "What could it be? Use it to find out!\n" 
  | AutoSinker900 -> 
    "If the AS900 is fired at a coordinate of an unsunk ship, it sinks" ^ 
    " the ship. Otherwise, it does nothing. * Note: You CAN fire at " ^ 
    "previously fired upon coordinates.\n"
  | MissFinder -> "Returns a coordinate that's guaranteed to be a miss.\n"
  | InventorySwapper -> "Swaps your inventory with the other player's.\n"
  | MoneyBag -> "What could be inside? One way to find out!\n"

let miss_finder_price = item_price MissFinder
let mystery_price = item_price MysteryItem

(** Representation of an item in the shop where the int correspond to the 
    price of the item in the shop*)
type shop_item = item * int

type player = {
  player_id: string;
  shots: (int*int) list;
  ships: ship list;
  inventory: item list;
  player_money: int;
}

type state = {
  p1:player;
  p2:player;
  p_shop: shop_item list;
}

let rec string_of_shop shop acc = 
  match shop with
  | [] -> acc
  | (i,price)::t -> 
    let str = (string_of_item i) ^ ", Cost = " ^ (string_of_int price) ^ "\n" 
              ^ acc in 
    string_of_shop t str

let rec string_of_inventory inventory acc = 
  match inventory with 
  | [] -> "Empty."
  | h::[] -> acc ^ (h |> string_of_item)
  | h::t -> string_of_inventory t ((h |> string_of_item) ^ ", " ^ acc)

exception OutOfBounds
exception DuplicateShot

let rec get_num_of_items acc (item_lst: item list) = 
  match item_lst with
  | [] -> acc
  | h::t -> get_num_of_items (acc + 1) t

let rec get_num_of_shots acc (shot_lst : (int * int) list) = 
  match shot_lst with
  | [] -> acc 
  | h::t -> get_num_of_shots (acc + 1) t

(* [ship_sunk ship] determines if a ship is sunk given a valid ship list **)
let rec ship_sunk ship = 
  match ship with 
  | [] -> true
  | (_,_,tf)::t -> if tf = false then false else ship_sunk t

let rec get_num_of_ships (counter: int) (ship_lst : ship list ) = 
  match ship_lst with
  | [] -> counter 
  | ship::t -> if ship_sunk ship then get_num_of_ships counter t 
    else get_num_of_ships (counter + 1) t

let rec ships_sunk (ship_list : ship list) =
  match ship_list with 
  | [] -> true
  | ship::t -> if ship_sunk ship then ships_sunk t else false

let rec ship_hit_number (ship_list : ship list) (counter : int) = 
  match ship_list with
  | [] -> counter
  | ship::t -> if ship_sunk ship = true then ship_hit_number t (counter + 1) 
    else ship_hit_number t counter

let rec is_ship_hit int_tuple ship  = 
  match ship with
  | [] -> false
  | (xs,ys,b)::t -> 
    match int_tuple with
    | (x,y) -> if (x=xs && y=ys) then true else is_ship_hit int_tuple t

(** [ship_thats_hit int_tuple ships] is Some [ship] if [int_tuple] shares the 
    same x,y as a coordinate in [ship] in [ships] does, and is None otherwise.*)
let rec ship_thats_hit int_tuple (ships : ship list )=
  match ships with 
  | [] -> None
  | h::t -> if is_ship_hit int_tuple h then Some h 
    else ship_thats_hit int_tuple t

let is_wounded_coordinate int_tuple ships =
  match ship_thats_hit int_tuple ships with
  | None -> false
  | Some ship -> 
    if ship_sunk ship then false else true 

let is_missed_shot int_tuple ships = 
  match ship_thats_hit int_tuple ships with
  | None -> true
  | Some ship -> false 

let is_sunk_shot int_tuple ships =
  match ship_thats_hit int_tuple ships with
  | None -> false 
  | Some ship -> if ship_sunk ship then true 
    else false

let coordinate_color int_tuple ships = 
  match ship_thats_hit int_tuple ships with
  | None -> cyan
  | Some ship -> if ship_sunk ship then black else red

let random_item () = 
  Random.self_init ();
  let num_items = List.length non_default_items in
  let random_index = Random.int num_items in
  List.nth non_default_items random_index

(** [colliding_ships ship1 ship2] is the veracity of the statement "[ship1] 
    and [ship2] have one or more overlapping points." *)
let rec colliding_ships ship1 ship2 =
  match ship1 with
  | [] -> false
  | h::t -> if List.mem h ship2 then true else colliding_ships t ship2 

(** [any_collisions_w_ship ship_list ship] is the veracity of the statements 
    "[ship] has one or more overlapping points with a ship in [ship_list]." *)
let rec any_collisions_w_ship ship_list ship =
  match ship_list with
  | [] -> false
  | h::t -> if (colliding_ships h ship) = true then true else 
      any_collisions_w_ship t ship

(** [build_ship x_start y_start x_end y_end bl] builds a ship with a starting
    x-coordinate of [x_start], a starting y-coordinate of [y_start], an ending
    x-coordinate of [x_end], and ending y-coordinate of [y_end]. 
    Raises: OutOfBounds if a player tries to make a diagonal ship (i.e. it must 
    hold that [x_start] equals [x_end] or [y_start] equals [y_end]) or if a 
    player tries to build a ship that goes off of the game board (i.e. 
    [x_start] and [y_start] must be greater than or equal to 1 and [x_end] and
    [y_end] must be less than or equal to [bl]) *)
let rec build_ship x_start y_start x_end y_end bl =
  if x_start < 1 || y_start < 1 || x_end < 1 || y_end < 1 || x_start > bl 
     || x_start > bl || y_start > bl || x_end > bl || y_end > bl 
     || (x_start = x_end || y_start = y_end) = false
  then raise OutOfBounds
  else if x_start = x_end && y_start = y_end then [(x_start,y_start,false)]
  else if x_start < x_end then 
    (x_start, y_start, false)::(build_ship (x_start+1) y_start x_end y_end bl)
  else if x_start > x_end then 
    (x_start, y_start, false)::(build_ship (x_start-1) y_start x_end y_end bl)
  else if y_start < y_end then
    (x_start, y_start, false)::(build_ship x_start (y_start+1) x_end y_end bl)
  else 
    (x_start, y_start, false)::(build_ship x_start (y_start-1) x_end y_end bl)

let add_ship player x_start y_start x_end y_end bl =
  let ship = build_ship x_start y_start x_end y_end bl in 
  match any_collisions_w_ship player.ships ship with
  | true -> raise OutOfBounds
  | false -> {player with ships = ship::player.ships}

(** [add_to_shot_list player shot] adds [shot] to [player]'s list of shots. 
    Raises: [DuplicateShot] if [shot] is already in [player]'s shot list. *)
let add_to_shot_list player shot =
  if List.mem shot player.shots then raise DuplicateShot 
  else if (let x,y = shot in x < 1 || x > 10 || y < 1 || y > 10) 
  then raise OutOfBounds
  else shot :: player.shots

(** [update_shot_ship shot ship acc counter] is [ship] after having been shot by
    [shot].
    Requires: [shot] overlaps with some point in [ship]. *)
let rec update_shot_ship shot ship acc counter = 
  match ship with 
  | [] -> failwith "This ship wasn't shot"
  | (x,y,tf)::t -> 
    match shot with 
    | (xs,ys) -> if x = xs && y = ys && counter = 0 then ((x,y,true) :: acc) @ t
      else update_shot_ship shot t ((x,y,tf)::acc) counter

(**[add_money player] helper function to add money to player ship every time 
   player successfully hit oponent ship *)
let add_money player = 
  let result = player.player_money + 10 in
  player.player_money = result

(** [update_shot_ships ship_list shot acc] is [ship_list] after [shot] occurs. 
    The list is built upon [acc]. *)
let rec update_shot_ships ship_list shot acc =
  match ship_list with
  | [] -> acc
  | ship::t -> if is_ship_hit shot ship 
    then ((update_shot_ship shot ship [] 0) :: acc ) @ t
    else update_shot_ships t shot (ship::acc)

(** [any_ship_shot ship_list shot] helper function to determine 
    if any ship was shot or not; return true if a ship was shot*)
let rec any_ship_shot shot ship_list =
  match ship_list with
  | [] -> false
  | ship::t -> if is_ship_hit shot ship then true else any_ship_shot shot t

(** [shot_payout shot ship_list] is the amount of money that a player gets for 
    firing [shot] at [ship_list]. The payout depends on if the shot hits 
    anything, hits nothing, or sinks a ship. *)
let shot_payout shot ship_list = 
  if is_sunk_shot shot ship_list then 3 
  else if any_ship_shot shot ship_list then 1
  else 2


let update_players player1 player2=
  {
    p1 = player1; 
    p2=player2; 
    p_shop = 
      [(MissFinder, miss_finder_price); (MysteryItem, mystery_price)]
  }

let rec afloat_ships ships afloat_acc =
  match ships with
  | [] -> afloat_acc
  | h::t -> if ship_sunk h then afloat_ships t afloat_acc
    else afloat_ships t (afloat_acc + 1)

(** [hits_on_ship ship acc] is the number of hits [ship] has taken. *)
let rec hits_on_ship ship acc = 
  begin
    match ship with 
    | [] -> acc
    | (_,_,tf)::t -> 
      if tf = true then hits_on_ship t (acc+1) 
      else hits_on_ship t acc
  end

let rec num_hits_total ships acc = 
  match ships with
  | [] -> acc 
  | h::t -> num_hits_total t (acc + hits_on_ship h 0)

let total_money player = 
  player.player_money

let rec enough_money shop_lst player i = 
  match shop_lst with 
  | [] -> raise ItemNotPresent
  | (h,p)::t -> 
    if  h = i then 
      if player.player_money >= p then true
      else false
    else enough_money t player i

(** [add_to_inv i player] is the player that results from [i] being added to 
    [player]'s inventory. *)
let add_to_inv i player = 
  {player with inventory = i::player.inventory}

let is_item_owned player i =
  if List.mem i player.inventory then true
  else false

(** [remove_one elt listy] removes the first occurence of [elt] from [listy].
    Requires: [elt] is in [listy]. *)
let rec remove_one elt listy = 
  match listy with
  | [] -> failwith "Prereq violated"
  | h::t -> if h = elt then t 
    else h::(remove_one elt t)

(** [remove_from_inv player i] is the player that results from [i] being 
    removed from [player]'s inventory. 
    Raises: [ItemNotPresent] if [i] isn't in [player]'s inventory. *)
let remove_from_inv player i = 
  if List.mem i player.inventory then 
    { player with inventory = remove_one i player.inventory }
  else raise ItemNotPresent

(** [remove_money player i] is the player that results from [player] buying [i].
    Requires: [player] must be able to afford [i]. *)
let remove_money player i = 
  {player with
   player_money = player.player_money - (item_price i)
  }

let purchase_results player i = 
  (add_to_inv i player) |> (fun x -> remove_money x i)

(** [remove_nondefault store_lst acc] is [store_lst] but without nondefault 
    items. Built upon [acc]. *)
let rec remove_nondefault store_lst acc =
  match store_lst with
  | [] -> acc 
  | (h,p)::t -> if List.mem h non_default_items 
    then remove_nondefault store_lst t
    else remove_nondefault t ((h,p)::acc)

let add_rnd_opt_to_store st = 
  let rndm = random_item () in 
  let rndm_price = item_price rndm in
  let remove_old = remove_nondefault st.p_shop [] in
  let new_store = (rndm, rndm_price)::remove_old in
  {st with p_shop = new_store}


let init_state p1id p2id = 
  {
    p1 = {
      player_id = p1id; 
      shots = []; 
      ships = []; 
      inventory = [MissFinder]; 
      player_money = 5;
    };
    p2 = {
      player_id = p2id; 
      shots = []; 
      ships = []; 
      inventory = [MissFinder]; 
      player_money = 5;
    };
    p_shop = [(MissFinder, miss_finder_price); (MysteryItem, mystery_price)]
  }

let shot_results shot shooting_player shot_player st = 
  let money_not_added =
    {
      p1 = {shooting_player with 
            shots = add_to_shot_list shooting_player shot; 
            player_money = shooting_player.player_money
           };
      p2 = {shot_player with 
            ships = 
              (update_shot_ships shot_player.ships shot [])
           };
      p_shop = st.p_shop
    } in 
  let new_money = 
    money_not_added.p1.player_money + shot_payout shot money_not_added.p2.ships 
  in {money_not_added
      with p1 = {money_not_added.p1 with player_money = new_money}}

(** [sink_ship_helper ship st] is the state resulting from [ship] in [st] being
    fired upon to the point of sinking by player 1. *)
let rec sink_ship_helper ship st = 
  match ship with
  | [] -> st
  | (x,y,tf)::t -> if tf = false 
    then (let new_st = shot_results (x,y) st.p1 st.p2 st in 
          sink_ship_helper t new_st) 
    else sink_ship_helper t st

let sink_ship coord st = 
  match ship_thats_hit coord st.p2.ships with
  | None -> st 
  | Some ship -> 
    let new_st = sink_ship_helper ship st in
    {new_st with p1 = {new_st.p1 with player_money = st.p1.player_money}}




