open Names
open! Base

module ImportsEnv : sig
  type t

  val import_messages : t -> ProtocolName.t -> t * string

  val import_channels : t -> ProtocolName.t -> t * string

  val import_result : t -> ProtocolName.t -> t * string

  (* These last 3 might be overkill *)
  val import_invitations : t -> t * string

  val import_callbacks : t -> t * string

  val import_roles : t -> t * string

  val generate_imports : t -> string
end

module ChannelEnv : sig
  type t

  val new_channel : t -> RoleName.t -> LabelName.t -> t * ChannelName.t

  val gen_channel_struct : t -> RoleName.t -> string

  val create : ProtocolName.t -> t
end

(* module InviteEnv : sig type t

   val new_send_invite_channel : t -> RoleName.t -> string -> RoleName.t ->
   ProtocolName.t -> t * string

   val new_recv_invite_channel : t -> RoleName.t -> string -> RoleName.t ->
   ProtocolName.t -> t * string

   val gen_channel_struct : t -> string

   val create : string -> t end *)

module CallbacksEnv : sig
  type t

  val new_send_callback : t -> LabelName.t -> RoleName.t -> t * string

  val new_recv_callback : t -> LabelName.t -> RoleName.t -> t * string

  (* Global protocol name *)

  val new_protocol_setup_callback : t -> ProtocolName.t -> t * string

  (* Local protocol name *)

  val new_create_protocol_env_callback :
    t -> string -> ProtocolName.t -> t * string

  val new_protocol_result_callback :
    t -> string -> ProtocolName.t -> RoleName.t -> t * string
end

val gen_code :
     Gtype.global_t
  -> Ltype.local_t
  -> (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t