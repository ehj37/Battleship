(** Raised when one tries to access an item that isn't present in a shop or 
    inventory. *)
exception ItemNotPresent

(** Raised when player 1 in the game's state cannot afford a given item. *)
exception NotEnoughMoney

(** Raised when one tries to build a diagonal ship or a ship with coordinates 
    that go past the limits of the game board.*)
exception OutOfBounds

(** Raised when a one tries adding an already present shot to a player's list of
    shots.*)
exception DuplicateShot

(* Type of Ships in BattleShip **)
type ship = (int*int*bool) list

type item = TripleShot | SquareShot | ShotSearcher3000 | MysteryItem 
          | MissFinder | AutoSinker900 | InventorySwapper | MoneyBag

(** Representation of an item in the shop where the int correspond to the 
    price of the item in the shop*)
type shop_item = item * int 

(* Representation of a Player **)
type player = {
  player_id: string;
  shots: (int*int) list;
  ships: ship list;
  inventory: item list;
  player_money: int
}

(** Representation of a state*)
type state = {
  p1:player;
  p2:player;
  p_shop: shop_item list;
}

(**[get_num_of_shots int (int*int) list is a helper function that determines 
   the number of shots in a player shot list] *)
val get_num_of_shots : int -> (int * int) list -> int

(**[get_num_of_shots int (int*int) list is a helper function that determines 
   the number of shots in a player shot list] *)
val get_num_of_items : int -> item list -> int

(** [get_num_of_ships int ships] determines and return the amount of ships the
    player has that are not sunk given [ships], the player's ships. *)
val get_num_of_ships : int -> ship list -> int 

(** [ship_sunk ship_list] checks to see if every ship of a player has been sunk. 
    Returns true if all values that match to each ship is true *)
val ships_sunk : ship list -> bool

(** [ship_hit_number ship_list counter] determines how many ships the player 
    hashit. Returns an int value based on how many true value
    in the ship * bool list*)
val ship_hit_number: ship list -> int -> int

(** [is_ship_hit int_tuple ship] is true if [int_tuple] shares the same x,y 
    coordinates as some point in [ship] and false otherwise.*)
val is_ship_hit: (int*int) -> ship -> bool

(** [is_wounded_coordinate int_tuple ships] is the veracity of the statement 
    "[int_tuple] corresponds to the coordinate of a ship that's been wounded in 
    [ships] (to be wounded is to have been hit but not sunk)."*)
val is_wounded_coordinate: (int*int) -> ship list -> bool

(** [is_missed_shot int_tuple ships] is [true] if [int_tuple] doesn't correspond
    to any part of any ship in [ships] and [false] otherwise. *)
val is_missed_shot: (int*int) -> ship list -> bool

(** [is_sunk_shot int_tuple ships] is [true] if [int_tuple] corresponds to a 
    coordinate in [ships] that is part of a ship that has been sunk, and false
    otherwise. *)
val is_sunk_shot: (int*int) -> ship list -> bool

(** [coordinate_color int_tuple ships shot_list] is the color of X that should 
    be present on the game window at coordinate [int_tuple]*)
val coordinate_color: (int*int) -> ship list -> Graphics.color

(** [init_state p1id p2id] is a state object with two players, the first having 
    name [p1id] and the second having name [p2id]. Neither player has any ships 
    or shots to start.*)
val init_state: string -> string -> state

(** [add_ship player x_start y_start x_end y_end bl] adds a ship to the list of 
    ships of [player] spanning from x-coordinate [x_start] to [x_end] and from 
    y-coordinate of [y_start] to [y_end]. [bl] is the length of the board in 
    terms of the number of ships. *)
val add_ship: player -> int -> int -> int -> int -> int -> player

(** [update_players player1 player2] creates a new state with players 
    [player1] and [player2]*)
val update_players: player -> player -> state

(** [shot_results shot shooting_player shot_player st] is the state resulting
    from [shot] being shot by [shooting_player] at [shot_player] in [st].
    Raises: [DuplicateShot] if [shot] is already in [shooting_player]'s shot 
    list. *)
val shot_results: (int*int) -> player -> player -> state -> state

(** [afloat_ships ships afloat_acc] is the number of ships in [ships] that have
    not yet been sunk. *)
val afloat_ships: ship list -> int -> int

(** [num_hits_total ships] is the number of shots that have connected with 
    ships in [ships]. *)
val num_hits_total: ship list -> int -> int

(**[total_money  player] returns the amount of money a player currently has *)
val total_money : player -> int

(** [enough_money shop_lst player i] is [i] if [i] is in [shop_lst] and [player]
    has enough money for it. 
    Raises: [ItemNotPresent] if [i] isn't in [shop_lst]. *)
val enough_money : shop_item list -> player -> item -> bool

(** [purchase_results player i] is the player that results from [player]
    purchasing [i]. 
    Requires: [player] must be able to afford [i]. *)
val purchase_results: player -> item -> player

(** [remove_from_inv player i] is the player that results from [i] being 
    removed from [player]'s inventory. 
    Raises: [ItemNotPresent] if [i] isn't in [player]'s inventory. *)
val remove_from_inv: player -> item -> player

(** [add_to_inv i player] is the player that results from [i] being added to 
    [player]'s inventory. *)
val add_to_inv: item -> player -> player

(** [string_of_item i] is a string representation of item [i].*)
val string_of_item: item -> string 

(** [string_of_shop shop acc] is a string representation of [shop], built upon
    [acc]. *)
val string_of_shop: shop_item list -> string -> string

(** [string_of_inventory inventory acc] is a string representation of the 
    inventory [inventory], built upon [acc].*)
val string_of_inventory: item list -> string -> string

(** [item_description i] is a string giving the description of [i]. *)
val item_description: item -> string

(** [random item ()] is a random, non-default item. *)
val random_item: unit -> item

(** [add_rnd_opt_to_store st] is the state resulting from any non-default 
    items being removed from [st]'s shop and a random non-default item being 
    added to [st]. *)
val add_rnd_opt_to_store: state -> state

(** [sink_ship coord st] is the state with any ship in [st] that has part of it 
    on [coord] fired upon by player 1 until sunk. *)
val sink_ship: int * int -> state -> state

