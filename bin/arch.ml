module type Sig = sig
  val name : string

  type register (* general purpose register *)

  val id_to_register : int -> register

  type instruction

  val to_concrete_instruction : Instruction.t -> instruction

  val print_asm : Format.formatter -> instruction -> unit
end