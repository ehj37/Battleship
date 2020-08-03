open Graphics
open State

module Tui = struct
  let window_length = 600
  let box_color = rgb 9 6 115 
  let text_color = black
  let sidebar_bc = rgb 9 6 115 
  let sidebar_fc = white
  let grid_color = black
  let grid_length = 400
  let grid_ll = (160, 100)
  let sidebar_ll_x = 10
  let sidebar_ll_y = 100
  let sidebar_ur_x = 120
  let ship_graphic_col = rgb 120 24 203
  let ship_graphic_ll_x = 75
  let ship_graphic_ll_y = 520

  (** [sqr_row num_sqr x_start y_pos length] draws [num_sqr] squares of side 
      length [length], with the first square's bottom left coordinate being 
      [x_start], [y_pos].*)
  let rec sqr_row num_sqr x_start y_pos length = 
    match num_sqr with 
    | 0 -> ()
    | _ -> 
      draw_rect x_start y_pos length length;
      sqr_row (num_sqr - 1) (x_start + length) y_pos length

  (** [number_grid_h x_pos y_pos length counter num_sqr] draws a row of numbers 
      from 1 to [num_sqr] below the grid's lower row of boxes at a height of 
      [y_pos]. Each number is half [length] away from the left edge of a box. 
      [counter] determines how many boxes still need to be numbered. 
      Requires: Default text size and font is used.*)
  let rec number_grid_h x_pos y_pos length counter num_sqr = 
    begin
      match counter with
      | 0 -> ()
      | _ -> 
        let half_length = length / 2 in 
        moveto (x_pos + half_length) y_pos;
        string_of_int (num_sqr - counter + 1) |> draw_string;
        number_grid_h (x_pos + length) y_pos length (counter - 1) num_sqr
    end

  (** [number_grid_h x_pos y_pos length counter num_sqr] draws a column of 
      numbers from 1 to [num_sqr] to the left the grid's left column of boxes 
      at a height of [y_pos]. Each number is 1/3 [length] away from the bottom 
      edge of a box. [counter] determines how many boxes still need to be 
      numbered. 
      Requires: Default text size and font is used.*)
  let rec number_grid_v x_pos y_pos length counter num_sqr = 
    begin
      match counter with
      | 0 -> ()
      | _ -> 
        let third_length = length / 3 in 
        moveto x_pos (y_pos + third_length);
        string_of_int (num_sqr - counter + 1) |> draw_string;
        number_grid_v x_pos (y_pos + length) length (counter - 1) num_sqr
    end 

  (** [number_grid num_sqr x_start y_start length] draws the correct numbers 
      that are meant to be placed on a grid with dimension of [num_sqr] by 
      [num_sqr], square length of [length], and lower left point of ([x_start],
      [y_start]). 
  *)
  let number_grid num_sqr x_start y_start length =
    number_grid_h x_start (y_start - 14) length num_sqr num_sqr;
    number_grid_v (x_start - 14) y_start length num_sqr num_sqr

  (** [grid_hlpr num_sqr counter x_start y_start length] draws and numbers a 
      grid with dimension of [num_sqr] by [num_sqr], square length of [length], 
      and lower left point of ([x_start], [y_start]).*)
  let rec grid_hlpr num_sqr counter x_start y_start length =
    begin
      match counter with 
      | 0 -> number_grid num_sqr x_start y_start length
      | _ -> 
        sqr_row num_sqr x_start ((counter - 1) * length + y_start) length;
        grid_hlpr num_sqr (counter - 1) x_start y_start length
    end

  (** [grid num_sqr x_ll y_ll length] creates a [num_sqr] by [num_sqr] grid 
      with cells of length [length]. The lower-left cell starts at coordinate 
      ([x_ll], [y_ll]). The grid is numbered on the horizontal and vertical. *)
  let grid num_sqr x_ll y_ll length =
    grid_hlpr num_sqr num_sqr x_ll y_ll length

  (** [box_text pos text_color box_color text] draws a text box with [text] in
      it at the current position. The box is of color [box_color]
      Requires: Default text size and font is used.*)
  let box_text text_col box_col text = 
    let text_dim = text_size text in 
    match current_point (), text_dim with 
    | (x,y), (width, height) ->
      set_color text_col;
      draw_string text;
      set_color box_col;
      draw_rect (x-2) (y-1) (width + 2) (height + 1)

  (** centers text within a pixel, returns x coordinate of bottom left point 
      that would make text centered.*)
  let center_text x_start text width = 
    match text_size text with
    | (x,_) -> x_start + (width - x) / 2 + 2

  (** [populate_sidebar sbc fc ll ur player_nm hits health en_ships fr_ships]
      draws a sidebar with lower-left hand corner at coordinate [ll] and upper-
      right hand corner at coordinate [ur]. Displayed on the sidebar is 
      [player_nm] (the player's name), [hits] (the number of hits the player has
      on the enemy), [health] (the number of hits the enemy has on the player),
      [en_ships] (the number of un-sunk ships and total ships of the enemy),
      and [fr_ships] (the number of un-sunk ships and total ships of the 
      player). The color of the sidebar's border [sbc] and its fill color is 
      [fc]. The color is set to be [foreground] at the function's conclusion.

      Requires: The difference between the y-coordinates of [ll] and [ul] must 
      be at least 250. The difference between the x-coordinates of [ll] and [ul]
      must be at least 120. The text must be deafault in size and font. Line 
      thickness must be default. *)
  let populate_sidebar sbc fc ll ur player_nm hits health en_ships fr_ships = 
    match ll, ur, en_ships, fr_ships with 
    | (x_ll, y_ll), (x_ur, y_ur), (en_left, en_tot), (fr_left, fr_tot) -> 
      let width = x_ur - x_ll in 
      let height = y_ur - y_ll in
      set_color sbc;
      draw_rect x_ll y_ll width height;
      set_color fc;
      fill_rect (x_ll + 1) (y_ll + 1) (width - 2) (height - 2);
      let cp_text = "CURRENT PLAYER:" in
      moveto (center_text x_ll cp_text width) (y_ur - (height * 1) / 15);
      box_text red box_color cp_text;
      moveto (center_text x_ll player_nm width) (y_ur - (height * 2) / 15);
      set_color text_color;
      draw_string player_nm;
      let hits_txt = "HITS ON ENEMY:" in
      moveto (center_text x_ll hits_txt width) (y_ur - (height * 4) / 15);
      box_text red box_color hits_txt;
      let hits_txt_2 = string_of_int hits in
      moveto (center_text x_ll hits_txt_2 width) (y_ur - (height * 5) / 15);
      set_color text_color;
      draw_string hits_txt_2;
      let health_txt = "HITS TAKEN:" in
      moveto (center_text x_ll health_txt width) (y_ur - (height * 7) / 15);
      box_text red box_color health_txt;
      let health_txt_2 = string_of_int health in
      moveto (center_text x_ll health_txt_2 width) (y_ur - (height * 8) / 15);
      set_color text_color;
      draw_string health_txt_2;
      let ship_txt = "ENEMY SHIPS LEFT:" in 
      moveto (center_text x_ll ship_txt width) (y_ur - (height * 10) / 15);
      box_text red box_color ship_txt;
      let ship_txt_2 = string_of_int en_left ^ " / " ^ string_of_int en_tot in
      moveto (center_text x_ll ship_txt_2 width) (y_ur - (height * 11) / 15);
      set_color text_color;
      draw_string ship_txt_2;
      let ship_txt_3 = "ALLY SHIPS LEFT:" in 
      moveto (center_text x_ll ship_txt_3 width) (y_ur - (height * 13) / 15);
      box_text red box_color ship_txt_3;
      let ship_txt_4 = string_of_int fr_left ^ " / " ^ string_of_int fr_tot in
      moveto (center_text x_ll ship_txt_4 width) (y_ur - (height * 14) / 15);
      set_color text_color;
      draw_string ship_txt_4;
      set_color foreground

  (** [conc_square board_col fill_col x_start y_start coord num_squares length]
      draws a smaller square within a square of length [length] on a grid with 
      [num_squares] squares on the vertical and horizontal that has a lower left
      corner at ([x_start],[y_start]). The square has border color [bord_col]
      and fill color [fill_col]. *)
  let conc_square bord_col fill_col x_start y_start coord num_squares length = 
    let len_frac = length / 7 in
    begin
      match coord with
      | (x,y) -> 
        let new_x = x_start + len_frac + length * (x-1) in
        let new_y = y_start + len_frac + length * (y-1) in
        let new_length = length - 2 * len_frac in
        set_color bord_col;
        draw_rect new_x new_y new_length new_length;
        set_color fill_col;
        fill_rect (new_x + 1) (new_y + 1) (new_length - 2) (new_length - 2)
    end

  (** [ship_graphic ship_color x_start y_start] draws sequence of strings that,
      combined, appear to be a ship and the word "BATTLESHIP". I'm pretty proud 
      of it honestly, it took about 10 minutes to make. The drawing begins at 
      coordinate ([x_start], [y_start]), and the color of it is [ship_color]. *)
  let ship_graphic ship_color x_start y_start = 
    set_color ship_color;
    let x = draw_string in
    moveto x_start (y_start + 36);
    x "   *                 ***   ****  ***  ***  *    ***  ***  * *  *  ***";
    moveto x_start (y_start + 30);
    x "   **                * *   *  *   *    *   *    *    *    * *  *  * *";
    moveto x_start (y_start + 24);
    x "   ***               ****  ****   *    *   *    **   ***  ***  *  ***";
    moveto x_start (y_start + 18);
    x "   *                 *  *  *  *   *    *   *    *      *  * *  *  *   ";
    moveto x_start (y_start + 12);
    x "   *                 ****  *  *   *    *   ***  ***  ***  * *  *  *";
    moveto x_start (y_start + 6);
    x "**************** ";
    moveto x_start (y_start);
    x " **************      ************************************************"

  (** [draw_board_hlpr num_squares player_nm hits health en_ships fr_ships]
      draws the game board with a grid of [num_squares] squares, and a sidebar
      populated by [player_nm], [hits], [health], [en_ships], and [fr_ships].*)
  let draw_board_hlpr num_squares player_nm hits health en_ships fr_ships = 
    open_graph " 600x600";
    match grid_ll with
    | (x, y) ->
      let square_length = (grid_length / num_squares) in
      set_window_title "Battleship";
      set_color grid_color;
      grid num_squares x y square_length;
      populate_sidebar sidebar_bc sidebar_fc (sidebar_ll_x, sidebar_ll_y) 
        (sidebar_ur_x, num_squares * square_length + sidebar_ll_y) 
        player_nm hits health en_ships fr_ships;
      ship_graphic ship_graphic_col ship_graphic_ll_x ship_graphic_ll_y;
      ship_graphic ship_graphic_col (ship_graphic_ll_x-2) ship_graphic_ll_y;
      ship_graphic ship_graphic_col (ship_graphic_ll_x+2) ship_graphic_ll_y

  (** [draw_shots ship_list shot_list num_squares ll sqr_len] draws all shots 
      in [shot_list] on the gameboard with length of [num_squares] squares. The 
      shots are colored according to the status and locations of ships in 
      [ship_list]. The grid's lower-left hand coordinate is [ll] and each square
      in the grid has length [sqr_len]. *)
  let rec draw_shots ship_list shot_list num_squares ll sqr_len =
    match shot_list, ll with 
    | (x,y)::t , (ll_x, ll_y) -> 
      let square_color = (coordinate_color (x,y) ship_list) in
      conc_square black square_color ll_x ll_y (x,y) num_squares sqr_len;
      draw_shots ship_list t num_squares ll sqr_len
    | _ -> ()

  (** [fill_row_w_shots row_num num_squares counter] is a list of shots for 
      each square in row [row_num] in a grid with [num_squares] squares on the
      horizontal and vertical. *)
  let rec fill_row_w_shots row_num num_squares counter = 
    if counter = num_squares then [] else 
      (counter+1, row_num)::(fill_row_w_shots row_num num_squares (counter+1))

  (** [fill_board_w_shots num_squares acc] is a list of shots for each square
      in a grid with [num_squares] squares on the horizontal and vertical. *)
  let rec fill_board_w_shots num_squares counter = 
    if num_squares = counter then [] else
      let c2 = counter + 1 in 
      fill_row_w_shots c2 num_squares 0 @ fill_board_w_shots num_squares c2

  (** [draw_board num_squares st], given [num_squares] and [st], draws the 
      gameboard.*)
  let draw_board num_squares st =
    let player_nm = st.p1.player_id in
    let hits = num_hits_total st.p2.ships 0 in
    let health = num_hits_total st.p1.ships 0 in
    let en_ships = (afloat_ships st.p2.ships 0, List.length st.p2.ships) in
    let fr_ships = (afloat_ships st.p1.ships 0, List.length st.p1.ships) in
    draw_board_hlpr num_squares player_nm hits health en_ships fr_ships;
    let square_length = grid_length / num_squares in
    draw_shots st.p2.ships st.p1.shots num_squares grid_ll square_length

  (** [grid num_sqr x_ll y_ll length] creates a [num_sqr] by [num_sqr] grid 
        with cells of length [length]. The lower-left cell starts at coordinate 
        ([x_ll], [y_ll]). The grid is numbered on the horizontal and vertical. *)

  (** [label_above_square text width] returns the x position that [text] should
      be drawn at above a square of width [width] for it to be centered. *)
  let label_above_square text width = 
    let x,y = text_size text in 
    (width / 2) - (x / 2)

  (** [unhit_ship_shots ship acc] is a list of coordinates in [ship] that 
      weren't hit. *)
  let rec unhit_ship_shots ship acc = 
    match ship with
    | [] -> acc
    | (x,y,tf)::t -> if tf = false then
        unhit_ship_shots t ((x,y)::acc)
      else unhit_ship_shots t acc

  (** [unhit_ships_shots ship acc] is a list of coordinates in [ship_list] that 
      weren't hit. *)
  let rec unhit_ships_shots ship_list acc = 
    match ship_list with 
    | [] -> acc
    | h::t -> unhit_ships_shots t ((unhit_ship_shots h []) @ acc)

  (** [draw_untaken_shots shot_list num_squares ll sqr_len] draws all shots in 
      [shot_list] on the gameboard with length of [num_squares] squares. The 
      shots are colored green. The grid's lower-left hand coordinate is [ll] 
      and each square in the grid has length [sqr_len]. *)
  let rec draw_untaken_shots shot_list num_squares ll sqr_len =
    match shot_list, ll with 
    | (x,y)::t , (ll_x, ll_y) -> 
      let square_color = green in
      conc_square black square_color ll_x ll_y (x,y) num_squares sqr_len;
      draw_untaken_shots t num_squares ll sqr_len
    | _ -> ()

  (** [end_screen st num_squares] draws the end screen in a game that concluded 
      at state [st] in which player 1 won. The grid has [num_squares] in the 
      game.*)
  let end_screen st num_squares = 
    let window_ht = window_length / 4 in 
    let square_length = (grid_length / num_squares) / 2 in
    let mini_grid_length = square_length * num_squares in 
    let whitespace = window_length - (2 * mini_grid_length) in
    let x1, y1 = (whitespace / 3, window_ht) in
    let x2, y2 = 
      (window_length - (whitespace / 3) - mini_grid_length, window_ht) in
    grid 10 x1 y1 square_length;
    grid 10 x2 y2 square_length;
    draw_shots st.p1.ships st.p2.shots 10 (x1,y1) square_length;
    draw_shots st.p2.ships st.p1.shots 10 (x2,y2) square_length;
    let unhit_p2_ships = unhit_ships_shots st.p2.ships [] in 
    let unhit_p1_ships = unhit_ships_shots st.p1.ships [] in
    draw_untaken_shots unhit_p2_ships 10 (x2,y2) square_length;
    draw_untaken_shots unhit_p1_ships 10 (x1,y1) square_length;
    let p1_ships_txt = st.p1.player_id ^ "'s ships" in
    let p2_ships_txt = st.p2.player_id ^ "'s ships" in
    let box_1_x_center = label_above_square p1_ships_txt mini_grid_length in
    moveto (box_1_x_center + x1) (y1+mini_grid_length+15);
    box_text red blue p1_ships_txt;
    let box_2_x_center = label_above_square p2_ships_txt mini_grid_length in
    moveto (box_2_x_center + x2) (y2+mini_grid_length+15);
    box_text red blue p2_ships_txt;
    let banner_txt_x_center = 
      label_above_square (st.p1.player_id ^ " has won.") window_length in 
    moveto banner_txt_x_center ((3*window_length)/4);
    box_text blue red (st.p1.player_id ^ " has won.")

end