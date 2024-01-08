open Format

type initial_value = Constant of int64
                   | VirtualAddress of string

let pp_initial_value fmt = function
  | Constant n -> fprintf fmt "extz(%#Lx, 64)" n
  | VirtualAddress address -> fprintf fmt "%s" address

module IntMap = Map.Make(Int)

type thread =
  {
    setup : initial_value IntMap.t;
    instructions: Instruction.t list
  }

let new_thread = { setup = IntMap.empty; instructions = [] }

type condition = RegisterAssertion of int * int64

type t =
  {
    threads : thread list;
    assertion : condition list;
  }