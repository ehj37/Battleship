
(* Parses Player Commands, Turn Play Command into Game Actions**)

(** The type [coordinates] represents the coordinates that the player wants to fire at.  
    Each element of the list represents a number on the coordinate plane that must 
    be in the dimension of the gameboard 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["fire 10 15"], then the coordinares is 
      [[10; 15]].
    - If the player command is ["fire 10     15"], then the coordinates is
      again [[10; 15]]. 
      An [coordinates] is not permitted to be the empty list. *)
type coordinates = (int*int)

(*TODO WRITE SPECS!!!!!!!!!!!!!!!!! **)
type location = (int * int * int * int)

(** The type [player_item] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of a player's item
    , where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["use shield"], then the object phrase is 
      [["shield"]].
    - If the player command is ["go      tower"], then the object phrase is
      again [["shield"]]. 
      An [player_item] is not permitted to be the empty list. *)

type player_item = string list
type shopping_item = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Quit
  | Fire of coordinates
  | CheckShips
  | Place of location
  | Inventory
  | Money
  | Shop
  | Use of State.item
  | Buy of State.item
  | TwoPlayer
  | OnePlayer
  | Easy
  | Medium
  | Hard
  | Instructions
  | Description of State.item

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the main command. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "Fire     10 15  "] is [Go [10; 15]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is not one of the command types,
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "Fire" and there is no coordinares following.*)
val parse : string -> command

