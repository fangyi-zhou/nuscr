(** Routed endpoint finite state machines *)

open Names

type action_ref =
  | RefA of string
  | EpsilonRef

(** Annotation for refined actions *)
type refinement_action_annot =
  { rec_expr_updates: (string * Expr.t) list
        (** List of updates to recursion variables *)
  ; tv_resets: TypeVariableName.t list
        (** List of recursion variables that should be reset
            because we've exited their scope. *) }
[@@deriving ord, sexp_of]
  
(** Transitions in the EFSM *)
type action =
  | SendA of RoleName.t * Gtype.message * refinement_action_annot
      (** Sending a [message] to [name] *)
  | RecvA of RoleName.t * Gtype.message * refinement_action_annot
      (** Receiving a [message] from [name] *)
  | Epsilon  (** Not used *)

(** Type of states in EFSM *)
type state = int

(** EFSM graph representation *)
module G :
  Graph.Sig.P
    with type V.t = state
     and type E.label = action_ref
     and type E.t = state * action_ref * state

(** Type of the EFSM *)
type t = G.t

val of_global_type : Gtype.t -> role:RoleName.t -> server:RoleName.t -> (state * t) * string

val show : t -> string
(** Produce a DOT representation of EFSM, which can be visualised by Graphviz *)
