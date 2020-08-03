open Graphics
open Gui
open Command
open State
open Tui
open Printf
open Items

(* [easy_st st] is the st after the easy CPU fires. **)
val easy_st : state -> state 

(* [medium_st st] is the st after the medium CPU fires. **)
val medium_st : state -> state

(** [set_cpu_ships ship_lengths player] is the computer player [player] but with
    added ships of lengths as given in [ship_lengths]. The ships are randomly
    placed. *)
val set_cpu_ships : int list -> player -> player

(* [hard_st st] is the st after the hard CPU fires. **)
val hard_st : state -> state
