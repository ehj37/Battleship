open OUnit2
open Command
open State
open Items

(*------------------------------TEST PLAN----------------------------------*)
(** In our OUnit Test Suite, We tested to ensure that the correct state was 
    returned after firing at a coordinate as well as the majority of our 
    functions in the module worked as intended. We tested modules Command, 
    State and Item. Functions in these modules were tested by writing test 
    functions that would compare one object to another, eg. beforeState to an 
    afterState. Our test cases fall under the category of black box testing in 
    which we just worry about the expected output and not the internal aspect 
    of the function. For moduels that wasn't tested in our OUnit test suit was 
    the cpu module and gui module in which was tested manually in the game. Our 
    OUnit Test suit proves the correctness of our system as it ensure the right 
    state is return asfter crucial component of the game like firing shot and 
    ensure that players objects (eg. ships, money etc..) are updated correctly. 
*)

(*------------------------------TEST CASES----------------------------------*)

(**[create_intial_state p1id p2id] is a helper function that creates the 
   intitial state *)
let create_intial_state p1id p2id = 
  State.init_state p1id p2id

(** The initial state at start of the game *)
let initstate = create_intial_state "player1" "player 2"

(** Creating ship and ships list *)
let ship1 = [(1,1,true)]
let ship2 = [(1,3, false)]
let ship3 = [(1,4,true)]
let ship4 = [(2,3,true)]
let ship5 = [(4,5,false)]
let edit_shiplst = [ship1; ship2]
let edit_shiplst2 = [ship1;]
let edit_shiplist3 = [ship1; ship2; ship3; ship4]
let editshiplst4 = [ship1; ship3;]
let editshiplst5 = [ship2; ship5;]

(**Representation of a player *)
let gucci_Player = {
  player_id = "GucciMane";
  shots = [];
  ships = [];
  inventory = [MissFinder];
  player_money = 0;
}

(**Representation of a player *)
let gucci_PlayerAfter = {
  player_id = "GucciMane";
  shots = [];
  ships = [];
  inventory = [];
  player_money = 0;
}

(**Representation of a player *)
let richgucci_PlayerAfter = {
  player_id = "GucciMane";
  shots = [];
  ships = [];
  inventory = [];
  player_money = 5;
}

(**Representation of a player*)
let player1 = {
  player_id = "Eric";
  shots = [];
  ships = [];
  inventory = [];
  player_money = 0;
}

(**Representation of a player*)
let player2 = {
  player_id = "Rukmini";
  shots = [];
  ships = [];
  inventory = [];
  player_money = 0;
}

(** Representation of a state*)
let state_gucci = {
  p1 = gucci_Player;
  p2 = gucci_PlayerAfter;
  p_shop = [(MissFinder,5)];
}

(**Player representation with the ship added *)
let player1_1ship = add_ship player1 1 1 3 1 10
let player2_1ship = add_ship player2 2 2 5 2 10


(** Representation of a state*)
let state1 = {
  p1 = player1_1ship;
  p2 = player2_1ship;
  p_shop = [];
}

(** Representation of states after a shot*)
let shot_st_1 = shot_results (2,2) state1.p1 state1.p2 state1
let shot_st_2 = shot_results (3,2) shot_st_1.p1 shot_st_1.p2 shot_st_1
let shot_st_3 = shot_results (4,2) shot_st_2.p1 shot_st_2.p2 shot_st_2
let shot_st_4 = shot_results (5,2) shot_st_3.p1 shot_st_3.p2 shot_st_3
let shot_st_5 = shot_results (10,10) state1.p1 state1.p2 state1

(**Representation of a player after shot is fired *)
let player1_hit_player2 = {
  player_id = "Eric";
  shots = [(2, 2)];
  ships = player1_1ship.ships;
  inventory = [];
  player_money = 1;
}

(**Representation of a player after cshot is fired *)
let player2_after_hit = {
  player_id = "Rukmini";
  shots = [];
  ships = [[(2, 2, true); (3, 2, false); (4, 2, false); (5, 2, false)]];
  inventory = [];
  player_money = 0;
}

(**Representation of a state after a hit *)
let state1_after_hit = {
  p1 = player1_hit_player2;
  p2 = player2_after_hit;
  p_shop = [];
}
(*----------------------------COMMAND TESTS---------------------------------*)
let test_commands  = [
  "Valid fire command"  >:: 
  (fun _ -> assert_equal (Fire (2,3))(parse "fire 2 3"));
  "Valid fire command"  >:: 
  (fun _ -> assert_equal (Fire (3,3))(parse "fire 3 3"));
  "Valid quit command" >::
  (fun _ -> assert_equal (Quit) (parse "quit"));
  "Valid checkship command">:: 
  (fun _ -> assert_equal (CheckShips)(parse "checkships"));
  "Valid inventory command">:: 
  (fun _ -> assert_equal (Inventory)(parse "inventory"));
  "Valid money command">:: 
  (fun _ -> assert_equal (Money)(parse "money"));
  "Valid shop command">:: 
  (fun _ -> assert_equal (Shop)(parse "shop"));
  "Valid two player command">:: 
  (fun _ -> assert_equal (TwoPlayer)(parse "two player"));
  "Valid one player command">:: 
  (fun _ -> assert_equal (OnePlayer)(parse "one player"));
  "Valid Easy level command">:: 
  (fun _ -> assert_equal (Easy)(parse "easy"));
  "Valid Medium level command">:: 
  (fun _ -> assert_equal (Medium) (parse "medium"));
  "Valid Hard level command">:: 
  (fun _ -> assert_equal (Hard)(parse "hard"));
  "Valid Instruction command">:: 
  (fun _ -> assert_equal (Instructions)(parse "instructions"));
  "Valid place ship command">:: 
  (fun _ -> assert_equal (Place (1, 1, 1,1))(parse "place 1 1 1 1 "));
  "Valid place ship command">:: 
  (fun _ -> assert_equal (Place (40, 40, 40,40))(parse "place 40 40 40 40 "));

]
(*--------------------------STATE TESTS-------------------------------------*)
let test_initstate = [
  "Valid ships init state" >::
  (fun _ -> assert_equal (get_num_of_ships 0 initstate.p1.ships) 0);
  "Valid money init state" >::
  (fun _ -> assert_equal (total_money initstate.p1) 5);
  "Valid shots init state" >::
  (fun _ -> assert_equal (get_num_of_shots 0 initstate.p1.shots) 0);
  "Valid inventory init state" >::
  (fun _ -> assert_equal (get_num_of_items 0 initstate.p1.inventory) 1);
  "Valid shop init state" >::
  (fun _ -> assert_equal (List.length initstate.p_shop) 2);

  "Valid ships init state" >::
  (fun _ -> assert_equal (get_num_of_ships 0 state1.p1.ships) 1);
  "Valid money init state" >::
  (fun _ -> assert_equal (total_money state1.p1) 0);
  "Valid shots init state" >::
  (fun _ -> assert_equal (get_num_of_shots 0 state1.p1.shots) 0);
  "Valid inventory init state" >::
  (fun _ -> assert_equal (get_num_of_items 0 state1.p1.inventory) 0);
  "Valid shop init state" >::
  (fun _ -> assert_equal (List.length state1.p_shop) 0);
]
(*------------------------FUNC TESTS-------------------------------------*)
let test_func = [
  "Test shot_results adding $ when player 1 shoots player 2's ship" >::
  (fun _ -> assert_equal (shot_st_1.p1.player_money) 1);
  "Test shot_results adding $ when player 1 sinks player 2's ship" >::
  (fun _ -> assert_equal (shot_st_4.p1.player_money) 6);
  "Test shot_results adding $ when player 1 misses player 2's ships" >::
  (fun _ -> assert_equal 2 (shot_st_5.p1.player_money));
  "Test Function Ships_Sunk Expected Result: False" >::
  (fun _ -> assert_equal (ships_sunk edit_shiplst) false);
  "Test Function Ships_Sunk Expected Result: False" >::
  (fun _ -> assert_equal (ships_sunk player2_after_hit.ships) false);
  "Test Function Ships_Sunk Expected Result: True" >::
  (fun _ -> assert_equal (ships_sunk edit_shiplst2) true);
  "Test Function Ship_Hit_Number Mutiple Ships" >::
  (fun _ -> assert_equal (ship_hit_number edit_shiplist3 0) 3);
  "Test Function Ship_Hit_Number" >::
  (fun _ -> assert_equal (ship_hit_number edit_shiplst 0) 1);
  "Test Function Ship_Hit_Number" >::
  (fun _ -> assert_equal (ship_hit_number player2_after_hit.ships 0) 0);
  "Test Is_Ship_Hit False" >::
  (fun _ -> assert_equal (is_ship_hit (4,5) ship1) false);
  "Test Is_Ship_Hit False2" >::
  (fun _ -> assert_equal (is_ship_hit (15,5) ship1) false);
  "Test Is_Ship_Hit True" >::
  (fun _ -> assert_equal (is_ship_hit (1,1) ship1) true);
  "Test Is_Ship_Hit True2" >::
  (fun _ -> assert_equal (is_ship_hit (1,4) ship3) true);
  "Test Is_Wounded_Coordinate False" >::
  (fun _ -> assert_equal (is_wounded_coordinate (1,1) edit_shiplst) false);
  "Test Is_Wounded_Coordinate True" >::
  (fun _ -> 
     assert_equal (is_wounded_coordinate (1,1) player1_1ship.ships) true);
  "Test add_ships valid ship" >::
  (fun _ -> assert_equal (add_ship player1 1 1 3 1 10) player1_1ship);
  "Test add_ships fully out of bounds ships" >::
  (fun _ -> 
     assert_raises (OutOfBounds) (fun() -> add_ship player1 15 20 25 20 10));
  "Test add_ships half out of bounds ships" >::
  (fun _ -> 
     assert_raises (OutOfBounds) (fun() -> add_ship player1 15 5 25 20 5));
  "Test add_ships on ship placed at the location of another ship" >::
  (fun _ -> 
     assert_raises (OutOfBounds) (fun() -> add_ship player1_1ship 1 1 3 1 5));
  "Test Afloat_Ships" >::
  (fun _ -> assert_equal (afloat_ships edit_shiplst 0) 1);
  "Test Num_Hits_Total" >::
  (fun _ -> assert_equal (num_hits_total edit_shiplst 0) 1);
  "Test Num_Hits_Total Initial State With No Hits" >::
  (fun _ -> assert_equal (num_hits_total editshiplst5 0) 0);
  "Test shot_results valid shot" >::
  (fun _ -> 
     assert_equal (state1_after_hit) 
       (shot_results (2, 2) player1_1ship player2_1ship state1));
  "Test shot_results Out of Bounds shot" >::
  (fun _ -> assert_raises (OutOfBounds) 
      (fun() -> shot_results (15, 15) player1_1ship player2_1ship state1));
  "Test shot_results Duplicate shot" >::
  (fun _ -> assert_raises (DuplicateShot) 
      (fun() -> shot_results 
          (2, 2) player1_hit_player2 player2_after_hit state1_after_hit));
  "Test remove inventory" >::
  (fun _ -> assert_raises (ItemNotPresent) 
      (fun() -> remove_from_inv (player1) MissFinder));
  "Test remove inventory" >::
  (fun _ -> assert_equal (remove_from_inv gucci_Player MissFinder) 
      gucci_PlayerAfter);
  "Test buy items" >::
  (fun _ -> assert_equal (purchase_results richgucci_PlayerAfter 
                            MissFinder) gucci_Player);
  "Test buy items when the player lacks the funds for a valid item" >::
  (fun _ -> assert_raises (NotEnoughMoney) (fun() -> 
       buy_item state_gucci MissFinder));
  "Test buy items when the item isn't in the store" >::
  (fun _ -> assert_raises (NotEnoughMoney) 
      (fun () -> buy_item state_gucci MissFinder));
  "Test coordinate_color for a hit ship coord" >::
  (fun _ -> 
     assert_equal Graphics.red (coordinate_color (2,2) shot_st_3.p2.ships));
  "Test coordinate_color for an sunk ship coord" >::
  (fun _ -> 
     assert_equal Graphics.black (coordinate_color (2,2) shot_st_4.p2.ships));
  "Test coordinate_color for a water coord" >::
  (fun _ -> 
     assert_equal Graphics.cyan (coordinate_color (10,10) shot_st_4.p2.ships));
]

let suite = "search test suite" >::: List.flatten [
    test_commands;
    test_initstate;
    test_func;
  ]
let _ = run_test_tt_main suite