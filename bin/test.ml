open Format

type initial_value = Constant of int64
                   | VirtualAddress of string


let pp_hex_constant fmt n =
  if n = Int64.zero then
    fprintf fmt "0x0"
  else
    fprintf fmt "%#Lx" n

let pp_initial_value fmt = function
  | Constant n -> fprintf fmt "extz(%a, 64)" pp_hex_constant n
  | VirtualAddress address -> fprintf fmt "%s" address

module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type thread =
  {
    setup : initial_value IntMap.t;
    instructions: Instruction.t list
  }

let new_thread = { setup = IntMap.empty; instructions = [] }

type condition = RegisterAssertion of int * int * int64

type t =
  {
    virtual_addresses : StringSet.t;
    physical_addresses : StringSet.t;
    initial_mappings : string StringMap.t;
    threads : thread list;
    assertion : condition list;
  }

let pp_assertion fmt = function
  | [] -> Format.fprintf fmt "<empty>"
  | conditions -> Format.fprintf fmt "%s" @@ String.concat " & " (List.map (fun (RegisterAssertion (proc, reg, v)) -> Printf.sprintf "%d:X%d = %Ld" proc reg v) conditions)

module Printer(Arch : Arch.Sig) = struct
  let pp_thread fmt index thread =
    Format.fprintf fmt "[thread.%d]\n" index;
    Format.fprintf fmt "code = \"\"\"\n";
    List.iter (fun instruction -> Arch.to_concrete_instruction instruction |> Format.fprintf fmt "    %a\n" Arch.print_asm) thread.instructions;
    Format.fprintf fmt "\"\"\"\n\n";

    Format.fprintf fmt "[thread.%d.reset]\n" index;
    IntMap.iter (fun reg value -> Format.fprintf fmt "R%d = \"%a\"\n" reg pp_initial_value value) thread.setup;
    Format.fprintf fmt "\n"

  let pp_test fmt test =
    Format.fprintf fmt "arch = \"%s\"\n" Arch.name;
    Format.fprintf fmt "name = \"PLACEHOLDER\"\n"; (* TODO *)
    Format.fprintf fmt "symbolic = [%s]\n\n" (StringSet.elements test.virtual_addresses |> List.map (fun va -> "\"" ^ va ^ "\"") |> String.concat ", ");

    Format.fprintf fmt "page_table_setup = \"\"\"\n";
    Format.fprintf fmt "    physical %s;\n" (StringSet.elements test.physical_addresses |> String.concat " ");
    StringMap.iter (Format.fprintf fmt "    %s |-> %s;\n") test.initial_mappings;
    StringSet.iter (Format.fprintf fmt "    *%s = 0;\n") test.physical_addresses;
    Format.fprintf fmt "\"\"\"\n\n";

    List.iteri (pp_thread fmt) test.threads;

    Format.fprintf fmt "[final]\n";
    Format.fprintf fmt "assertion = \"%a\"" pp_assertion test.assertion
end