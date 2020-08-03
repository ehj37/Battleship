open Graphics
open Gui
open Command
open State
open Tui
open Printf
open Items

(** [hax_hit_odds] when divided by 10 is the probability of the CPU using hax 
    (AKA looking into the player's list of ships and firing at a shot in it.)*)
let hax_hit_odds = 2

(* [shoot_randomly st] is the state after the computer player fires at a random 
   coordinate on the board with initial state [st]. **)
let rec shoot_randomly st = 
  Random.self_init ();
  let x = (Random.int 10) + 1 in
  let y = (Random.int 10) + 1 in 
  let command = "fire " ^ (string_of_int x) ^ " " ^ (string_of_int y) ^ "\n" in 
  match shot_results (x,y) st.p1 st.p2 st with
  | new_st -> 
    print_string command;
    new_st
  | exception DuplicateShot -> shoot_randomly st


(* [easy_st st] is the st after the easy CPU fires. **)
let easy_st st = 
  shoot_randomly st

let rec set_cpu_ships ship_lengths player =
  match ship_lengths with
  | [] -> player 
  | h::t -> 
    Random.self_init ();
    let x1 = (Random.int 10) + 1 in 
    let y1 = (Random.int 10) + 1 in
    let is_vert = Random.bool () in 
    let is_down_or_left = Random.bool () in 
    if is_vert then
      if is_down_or_left then
        (try (let new_player = add_ship player x1 y1 x1 (y1-h+1) 10 in 
              set_cpu_ships t new_player)
         with OutOfBounds -> set_cpu_ships ship_lengths player)
      else 
        (try (let new_player = add_ship player x1 y1 x1 (y1+h-1) 10 in 
              set_cpu_ships t new_player)
         with OutOfBounds -> set_cpu_ships ship_lengths player)
    else if is_down_or_left 
    then (try (let new_player = add_ship player x1 y1 (x1-h+1) y1 10 in 
               set_cpu_ships t new_player)
          with OutOfBounds -> set_cpu_ships ship_lengths player)
    else (try (let new_player = add_ship player x1 y1 (x1+h-1) y1 10 in 
               set_cpu_ships t new_player)
          with OutOfBounds -> set_cpu_ships ship_lengths player)

(** [find_wounded_targets shots_list ship_list acc] is a list of the shots in 
    [shots_list] that correspond to coordinates of "wounded" spots in 
    [ship_list] (a wounded spot has been hit, but the ship it's a part of has 
    not been sunk). The list is built upon [acc]. *)
let rec find_wounded_targets shots_list ship_list acc =
  match shots_list with
  | [] -> acc
  | shot::t -> if is_wounded_coordinate shot ship_list 
    then find_wounded_targets t ship_list (shot::acc) 
    else find_wounded_targets t ship_list acc

(** [adjacent_wounds (x,y) wounded_list] is a list of all coordinates in 
    [wounded_list] that are adjacent to coordinate [(x,y)].*)
let adjacent_wounds (x,y) wounded_list =
  let up = (x,y+1) in 
  let down = (x,y-1) in 
  let left = (x-1, y) in 
  let right = (x+1, y) in
  List.filter (fun x -> List.mem x wounded_list) [up;down;left;right]

(** [pick_target listy wounded_list acc] is a coordinate
    Requires: for the initial call of the function, [listy] and [wounded_list] 
    must be non-empty lists with the same contents. *)
let rec pick_target listy wounded_list acc = 
  match listy with
  | [] -> acc
  | (x,y)::t -> if List.length (adjacent_wounds (x,y) wounded_list) > 0
    then (x,y)
    else pick_target t wounded_list (x,y)

(** [Splash] is raised when a shot is either off the grid, a previously taken 
    shot that was a miss, or a shot that contributed to a ship having already
    sunk.*)
exception Splash

(** [search_dir (x,y) st direction] is the coordinate on the board going
    in [direction] from [(x,y)] if going in [direction] if the path going in 
    [direction] has some number of wounded coordinates and then an unfired upon 
    coordinate. 
    Raises: [Splash] if the path going in [direction] has some number of wounded
    coordinates and then a fired upon but not wounded coordinate, or if an 
    invalid direction is given.*)
let rec search_dir (x,y) st direction = 
  if x < 1 || x > 10 || y > 10 || y < 1 then raise Splash else
    let ships = st.p2.ships in 
    let shots = st.p1.shots in
    match direction with
    | "up" -> if List.mem (x,y) shots = false then (x,y) 
      else if is_missed_shot (x,y) ships || is_sunk_shot (x,y) ships
      then raise Splash 
      else search_dir (x,y+1) st direction
    | "down" -> if List.mem (x,y) shots = false then (x,y) 
      else if is_missed_shot (x,y) ships || is_sunk_shot (x,y) ships
      then raise Splash 
      else search_dir (x,y-1) st direction
    | "left" -> if List.mem (x,y) shots = false then (x,y) 
      else if is_missed_shot (x,y) ships || is_sunk_shot (x,y) ships
      then raise Splash 
      else search_dir (x-1,y) st direction
    | "right" -> if List.mem (x,y) shots = false then (x,y) 
      else if is_missed_shot (x,y) ships || is_sunk_shot (x,y) ships
      then raise Splash 
      else search_dir (x+1,y) st direction
    | _ -> raise Splash

(** [rndm_surrounding_coord (x,y) st] is a random unfired upon coordinate on 
    the grid in [st] that's above, below, to the left of, or to the right of 
    [(x,y)].
    Requires: [(x,y)] must be a wounded coordinate. *)
let rec rndm_surrounding_coord (x,y) st = 
  Random.self_init ();
  let dir_list = ["up";"down";"left";"right"] in 
  let rnd_index = Random.int 4 in
  let direction = List.nth dir_list rnd_index in
  try search_dir (x,y) st direction 
  with Splash -> rndm_surrounding_coord (x,y) st

(** [killmode st (x,y) adj_wounds acc] is the coordinate that should be fired
    upon in [st] when [(x,y)] is a wounded coordinate and [adj_wounds] are 
    all coordinates adjacent to [(x,y)] that are wounded.
    Requires: The first call to this function should have [adj_wounds] and [acc]
    have identical contents. *)
let rec killmode st (x,y) adj_wounds acc=
  if adj_wounds = [] then 
    rndm_surrounding_coord (x,y) st
  else if List.mem (x,y+1) acc then 
    (try search_dir (x,y+1) st "up" with Splash -> 
       let listy = List.filter (fun w -> (w = (x,y+1)) = false) acc in
       killmode st (x,y) adj_wounds listy)
  else if List.mem (x,y-1) acc then 
    (try search_dir (x,y-1) st "down" with Splash -> 
       let listy = List.filter (fun w -> (w = (x,y-1)) = false) acc in
       killmode st (x,y) adj_wounds listy)
  else if List.mem (x-1,y) acc then 
    (try search_dir (x-1,y) st "left" with Splash -> 
       let listy = List.filter (fun w -> (w = (x-1,y)) = false) acc in
       killmode st (x,y) adj_wounds listy)
  else if List.mem (x+1,y) acc then 
    (try search_dir (x+1,y) st "right" with Splash -> 
       let listy = List.filter (fun w -> (w = (x+1,y)) = false) acc in
       killmode st (x,y) adj_wounds listy)
  else if List.mem (x,y+1) adj_wounds then 
    (try search_dir (x,y+1) st "up" with Splash -> 
       let listy = List.filter (fun w -> (w = (x,y+1)) = false) adj_wounds in
       killmode st (x,y) listy acc)
  else if List.mem (x,y-1) adj_wounds then 
    (try search_dir (x,y-1) st "down" with Splash -> 
       let listy = List.filter (fun w -> (w = (x,y-1)) = false) acc in
       killmode st (x,y) listy acc)
  else if List.mem (x-1,y) adj_wounds then 
    (try search_dir (x-1,y) st "left" with Splash -> 
       let listy = List.filter (fun w -> (w = (x-1,y)) = false) acc in
       killmode st (x,y) listy acc)
  else if List.mem (x+1,y) adj_wounds then 
    (try search_dir (x+1,y) st "right" with Splash -> 
       let listy = List.filter (fun w -> (w = (x+1,y)) = false) acc in
       killmode st (x,y) listy acc)
  else failwith "Well, how did that happen?"

let medium_st st = 
  let wound_list = find_wounded_targets st.p1.shots st.p2.ships [] in
  if List.length wound_list = 0 then shoot_randomly st
  else match pick_target wound_list wound_list (-99,-99) with
    | (x,y)  ->
      let adj_wounds = (adjacent_wounds (x,y) wound_list) in 
      let target_x, target_y = killmode st (x,y) adj_wounds adj_wounds in
      let command = "fire " ^ (string_of_int target_x) ^ " " 
                    ^ (string_of_int target_y) ^ "\n" in 
      print_string command;
      shot_results (target_x, target_y) st.p1 st.p2 st

(** [rnd_hit_shot st] is a random coordinate in [st] that, when fired at by 
    player 1, would result in a hit. *)
let rnd_hit_shot st = 
  Random.self_init ();
  let hits = potential_hits st in 
  let rnd_index = Random.int (List.length hits) in 
  List.nth hits rnd_index

(** [max_length list_list acc] is the size of the list with the largest length
    in [list_list], and [acc] if [list_list] is empty. *)
let rec max_length list_list acc = 
  match list_list with
  | [] -> acc
  | listy::ll -> 
    if List.length listy > acc then max_length ll (List.length listy)
    else max_length ll acc

(** [fire_rnd_in_quadrant st] is the state resulting from a random shot being
    taken in the least shot-upon quadrant in [st]. If multiple quadrants are 
    tied for being the least shot-upon then a coordinate from a random one of
    the quadrants is taken. *)
let fire_rnd_in_quadrant st = 
  let potential_shots = not_in_shot_list st in 
  let q1_shots = List.filter (fun (x,y) -> x > 5 && y > 5) potential_shots in 
  let q2_shots = List.filter (fun (x,y) -> x < 6 && y > 5) potential_shots in 
  let q3_shots = List.filter (fun (x,y) -> x < 6 && y < 6) potential_shots in 
  let q4_shots = List.filter (fun (x,y) -> x > 5 && y < 6) potential_shots in
  let quads_list = [q1_shots;q2_shots;q3_shots;q4_shots] in
  let max_len = max_length quads_list 0 in 
  let least_shot_quads = 
    List.filter (fun x -> List.length x = max_len) quads_list in 
  Random.self_init ();
  let rnd_index = Random.int (List.length least_shot_quads) in 
  let quadrant = List.nth least_shot_quads rnd_index in 
  let rnd_index_quad = Random.int max_len in
  let x,y = List.nth quadrant rnd_index_quad in
  let command = "fire " ^ (string_of_int x) ^ " " ^ (string_of_int y) ^ "\n" in
  print_string command;
  shot_results (x,y) st.p1 st.p2 st



(** [okay_hax st] is the state resulting from a shot being taken with "hax" 
    implemented in [st]. What is "hax," you ask? Well, there's a 10% chance 
    that the shot fired will be a hit, and a 90% chance that the shot will be
    fired in the least-fired upon quadrant. *)
let okay_hax st = 
  Random.self_init ();
  let rand_or_not = Random.bool () in
  let rand_int = Random.int 10 in 
  if rand_int = 0 
  then (let x,y = rnd_hit_shot st in print_string 
          ("fire " ^ (string_of_int x) ^ " " ^ (string_of_int y) ^ "\n");
        shot_results (x,y) st.p1 st.p2 st)
  else if rand_or_not then shoot_randomly st else fire_rnd_in_quadrant st

let hard_st st = 
  let wound_list = find_wounded_targets st.p1.shots st.p2.ships [] in
  if List.length wound_list = 0 then okay_hax st
  else match pick_target wound_list wound_list (-99,-99) with
    | (x,y)  ->
      let adj_wounds = (adjacent_wounds (x,y) wound_list) in 
      let target_x,target_y = killmode st (x,y) adj_wounds adj_wounds in
      let command = "fire " ^ (string_of_int target_x) ^ " " ^ 
                    (string_of_int target_y) ^ "\n" in 
      print_string command;
      shot_results (target_x, target_y) st.p1 st.p2 st