open Names

(** Global types *)

type payload =
  | PValue of VariableName.t option * PayloadTypeName.t
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, show, ord]

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, show, ord]

(** The type of global types *)
type t =
  | MessageG of message * RoleName.t * RoleName.t * t
      (** [MessageG (msg, sender, receiver, t)] starts by sending message
          [msg] from [sender] to [receiver] and continues as [t] *)
  | MuG of TypeVariableName.t * t  (** Fixpoint *)
  | TVarG of TypeVariableName.t  (** Recursive variable *)
  | ChoiceG of RoleName.t * t list
      (** [ChoiceG (name, ts)] expresses a choice located at participant
          [name] between the [ts] *)
  | EndG  (** Empty global type *)

val show : t -> string
(** Provides a textual representation of a global type *)

val of_protocol : Syntax.global_protocol -> t
(** Turn a raw protocol (from the parser) into a global type *)

val normalise : t -> t
(** Normalise a global type. This mainly collapses nested choice on the same
    participant and unfolds fixpoints *)
