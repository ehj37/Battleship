open Graphics
open Command
open State
open Gui
open Tui
open Printf
open Cpu
open Items

let instructions = "Type 'fire x y' to fire at (x,y). \n" ^ 
                   "Type 'quit' to quit. \n" ^
                   "Type 'money' to see how much money you have. \n" ^
                   "Type 'shop' to see the shop's inventory. \n" ^
                   "Type 'buy 'i'' to buy item 'i' from the shop. \n" ^
                   "Type 'description 'i'' to see what item 'i' does. \n"

(** [check_won st] is true if p1 has won in [st] and false otherwise. *)
let check_won st = 
  if ships_sunk st.p2.ships then true else false

let win_procedure st  = 
  print_string "You win! Type anything to end the game. \n";
  ignore (read_line ());
  clear_graph ();
  Tui.end_screen st 10;
  print_string "Type anything to exit. \n";
  ignore (read_line ());
  exit 0

(* [process_commands st] analyze the user inputed commands and perform in game 
   action based on those inputs. Waits until state has been altered, returns
   state. FIXED: waits until player's turn is over. **)
let rec process_commands st = 
  begin
    match parse (read_line ()) with
    | Quit -> 
      exit 0
    | CheckShips -> 
      let result = string_of_int (get_num_of_ships 0 st.p1.ships) in
      print_string(result ^ "\n"); 
      process_commands st
    | Fire coordinate -> 
      clear_graph ();
      (shot_results coordinate st.p1 st.p2 st)
    | Place c -> 
      print_string "Cannot be done during game \n";
      process_commands st
    | Inventory -> 
      print_string (string_of_inventory st.p1.inventory "" ^ "\n");
      print_string "Enter a command. \n";
      process_commands st
    | Money -> 
      let result = string_of_int(total_money st.p1) in
      print_string(result ^ "\n"); 
      print_string "Enter a command. \n";
      process_commands st
    | Shop -> 
      print_string (string_of_shop st.p_shop "");
      print_string "Enter a command. \n";
      process_commands st
    | Use i -> 
      (try (let i_st = use_item st i in 
            clear_graph ();
            draw_board 10 i_st;
            set_color black;
            if check_won i_st then win_procedure i_st else
              (print_string "Enter a command. \n";
               process_commands i_st)) 
       with ItemNotPresent -> 
         print_string "That isn't in your inventory! Try something else.\n"; 
         process_commands st)
    | Buy i -> (try let st1 = buy_item st i in process_commands st1 with 
        | NotEnoughMoney -> 
          (print_string "You're too poor! Try something else!\n";
           process_commands st)
        | ItemNotPresent -> 
          (print_string "That item isn't in the shop! Try something else.\n";
           process_commands st))
    | exception Empty -> 
      print_string "Empty command, try again. \n";
      process_commands st
    | Instructions -> 
      print_string instructions;
      print_string "Enter a command. \n";
      process_commands st
    | Description i -> 
      print_string (item_description i);
      print_string "Enter a command. \n";
      process_commands st
    | exception Malformed -> 
      print_string "Malformed command, try again. \n";
      process_commands st
    | _ -> 
      print_string "Cannot be done during game \n";
      process_commands st
  end

(** [ai_state difficulty st] is the state after the computer (at difficulty 
    level [difficulty]) takes its turn. The state before the computer takes its 
    turn is [st].*)
let cpu_state difficulty st = 
  if difficulty = "easy" 
  then easy_st st
  else if difficulty = "medium"
  then medium_st st
  else if difficulty = "hard"
  then hard_st st
  else failwith "Wow, a new difficulty! How did that happen?"

(** [gameplay st is_two_p ai_turn difficulty ref_shop] runs the game after its 
    state [st] has been set up. The game is two-player if [is_two_p] and 
    one-player otherwise. If the game is one-player then [difficulty] determines 
    the CPU's difficulty, and [cpu_turn] is true if it's the CPU's turn and 
    false otherwise. The shop is refreshed from [st] if [ref_shop] and isn't
    otherwise. *)
let rec gameplay st is_two_p cpu_turn difficulty ref_shop = 
  let st2 = (if ref_shop then add_rnd_opt_to_store st else st) in
  clear_graph ();
  draw_board 10 st2;
  print_string (st2.p1.player_id ^ "'s turn.\n");
  print_string "Enter a command. \n";
  if cpu_turn then 
    let new_st = cpu_state difficulty st2 in
    clear_graph ();
    draw_board 10 new_st;
    if ships_sunk new_st.p2.ships = true 
    then (print_string "You've lost. It happens to the best of us.\n";
          print_string "Type anything to end the game.\n";
          ignore (read_line ());
          clear_graph ();
          Tui.end_screen new_st 10;
          print_string "Type anything to exit.\n";
          ignore (read_line ());
          exit 0)
    else print_string "Type anything to go to the next player's turn. \n";
    ignore (read_line ());
    let next_turn_st = update_players new_st.p2 new_st.p1 in
    gameplay next_turn_st is_two_p false difficulty true
  else
    match process_commands st2 with
    | new_st -> 
      clear_graph ();
      draw_board 10 new_st;
      if ships_sunk new_st.p2.ships = true 
      then win_procedure new_st
      else print_string "Type anything to go to the next player's turn. \n";
      ignore (read_line ());
      let next_turn_st = update_players new_st.p2 new_st.p1 in
      if is_two_p 
      then gameplay next_turn_st is_two_p cpu_turn difficulty true
      else gameplay next_turn_st is_two_p true difficulty true
    | exception OutOfBounds -> 
      print_string "Shot out of bounds. Try again.\n";
      gameplay st2 is_two_p cpu_turn difficulty false
    | exception (Failure _) -> 
      print_string "Try something else!\n";
      gameplay st2 is_two_p cpu_turn difficulty false
    | exception DuplicateShot -> 
      print_string "You've already fired there! Try again.\n";
      gameplay st2 is_two_p cpu_turn difficulty false
    | exception ItemNotPresent -> 
      print_string "That item isn't there! Enter a different command. \n";
      gameplay st2 is_two_p cpu_turn difficulty false
    | exception Empty -> print_string "Type something! \n";
      gameplay st2 is_two_p cpu_turn difficulty false
    | exception Malformed ->
      print_string "Malformed command. Try again. \n";
      gameplay st2 is_two_p cpu_turn difficulty false


(** [process_mode] is true if the user selects two player and false if one 
    player is selected *)
let rec process_mode () = 
  print_string "Enter Command to select game mode \n";
  match parse (read_line ()) with
  | TwoPlayer -> true
  | OnePlayer -> false
  | exception _ | _ -> 
    print_string "Invalid command, try again \n";
    process_mode ()

(** [process_difficulty ()] is a string with the difficult the user
  * selects for the game *)
let rec process_difficulty () = 
  print_string "Enter Difficulty \n Type in easy, medium, or hard.\n";
  match parse (read_line ()) with 
  | Easy -> "easy"
  | Medium -> "medium"
  | Hard -> "hard"
  | exception _ | _ -> 
    print_string "Invalid command, try again \n";
    process_difficulty ()



(** [set_ship ship_length player] is [player] but with an added ship of length
    [ship_length]. The player determines the ship's location on the grid. *)
let rec set_ship ship_length player = 
  match parse (read_line ()) with 
  | Place (x1, y1, x2, y2) -> 
    if (abs (x1-x2)) - ship_length +1 = 0 || (abs (y1-y2)) - ship_length +1 = 0 
    then (try add_ship player x1 y1 x2 y2 10
          with OutOfBounds -> 
            (print_string "Improper ship placement! Try again!\n";
             set_ship ship_length player))
    else (print_string "That ship has the wrong length! Try again!\n";
          set_ship ship_length player)
  | Quit -> exit 0
  | exception _ -> 
    print_string "Improper ship placement! Try again!\n";
    set_ship ship_length player
  | _ -> 
    print_string "Improper ship placement! Try again!\n";
    set_ship ship_length player


(** [set_ships acc_player ship_lengths] is [acc_player] but with added ships
    of lengths specified by the contents of [ship_lengths]. The player
    determines the location of each ship. *)
let rec set_ships acc_player ship_lengths = 
  match ship_lengths with 
  | [] -> acc_player
  | h::t -> 
    let ll = Tui.grid_ll in
    draw_shots acc_player.ships (Tui.fill_board_w_shots 10 0) 10 ll 40;
    print_string ("Enter Command to set the ship of length " ^ 
                  (string_of_int h) ^"\n");
    let new_acc = set_ship h acc_player in
    draw_shots new_acc.ships (Tui.fill_board_w_shots 10 0) 10 ll 40;
    set_ships new_acc t

(** [process_name is_p1] is player 1's name (as input by the user) if [is_p1]
    and is player 2's name otherwise. A valid name has a length greater than 0 
    but less than 17 characters. Leading and tailing whitespace is removed. *)
let rec process_name is_p1 =
  let player_num = if is_p1 then "player 1" else "player 2" in
  let prompt_message = "Type in " ^ player_num ^ "'s name : \n" in
  ANSITerminal.(print_string [red] prompt_message);
  let p1_name = read_line () |> String.trim in
  if String.length p1_name = 0 then
    (print_string "That's an... interesting name... try again.\n";
     process_name is_p1)
  else if String.length p1_name > 17 then
    (print_string "Name exceeds length limit of 17 characters. Try again.\n";
     process_name is_p1)
  else p1_name



(** [set_up_game is_two_p difficulty] is the state that results after player 
    name(s) have been entered in a game of difficulty [difficulty] that's 
    two-player is [is_two_p] and one player otherwise. *)
let set_up_game is_two_p difficulty =
  match is_two_p with
  | true ->
    let p1_name = process_name true in
    let p2_name = process_name false in
    init_state p1_name p2_name
  | false -> 
    let p1_name = process_name true in
    if difficulty = "easy" then init_state p1_name "EasyBot 4000"
    else if difficulty = "medium" then init_state p1_name "MediumBot 6000"
    else if difficulty = "hard" then init_state p1_name "HardBot 8000"
    else failwith "invalid difficulty"



(** [set_up_ships acc_st is_two_p difficulty ship_lengths] is [acc_st] but with
    ships of lengths [ship_lengths] added in for each player in a game that's 
    two-player if [is_two_p] and one-player otherwise and has a difficulty of 
    [difficulty]. *)
let set_up_ships acc_st is_two_p difficulty ship_lengths =
  match is_two_p, ship_lengths with 
  | _, [] -> acc_st
  | true, h::t -> 
    print_string "Placement instructions: Type place x1 y1 x2 y2 to place a 
    ship from (x1,y1) to (x2,y2).\n";
    print_string ("Player 1, Place Your Ships \n");
    let player_1 = set_ships acc_st.p1 ship_lengths in 
    print_string "Type anything to go to player 2's ship placement.\n";
    ignore (read_line ());
    ANSITerminal.erase Above;
    print_string "Placement instructions: Type place x1 y1 x2 y2 to place a 
    ship from (x1,y1) to (x2,y2).\n";
    print_string("Player 2, Place Your Ships \n");
    let player_2 = set_ships acc_st.p2 ship_lengths in 
    print_string "Type anything to begin the game.\n";
    ignore (read_line ());
    ANSITerminal.erase Above;
    update_players player_1 player_2
  | false, h::t -> 
    print_string "Placement instructions: Type place x1 y1 x2 y2 to place a 
    ship from (x1,y1) to (x2,y2).\n";
    (print_string ("Player 1, Place Your Ships \n");
     let player_1 = set_ships acc_st.p1 ship_lengths in
     print_string "Type anything to begin the game.\n";
     ignore (read_line ());
     ANSITerminal.erase Above;
     let player_2 = set_cpu_ships ship_lengths acc_st.p2 in 
     update_players player_1 player_2)


(** [main ()] prompts for the game to play, then starts it. *)
let main () = 
  ANSITerminal.
    (print_string [red] "Welcome to Battleship!\n
    First type 'one player' or 'two player'\n");
  let is_two_p = process_mode () in
  let decide_diff is_2 =
    if is_2 then "easy" else (process_difficulty ()) in
  let difficulty = decide_diff is_two_p in
  let start_st = set_up_game is_two_p difficulty in
  ANSITerminal.
    (print_string [red] ("Time to place ships! \n" ));
  Tui.draw_board 10 start_st;
  Tui.draw_shots 
    start_st.p1.ships (Tui.fill_board_w_shots 10 0) 10 Tui.grid_ll 40;
  let gameready_st = set_up_ships start_st is_two_p difficulty [3;3;4;5] in
  ANSITerminal.
    (print_string [red] 
       ("So it begins!\n" ^ 
        "Type 'instructions' for instructions at any point. \n"));
  gameplay gameready_st is_two_p false difficulty true



(* Execute the game engine. *)
let () = main ()
