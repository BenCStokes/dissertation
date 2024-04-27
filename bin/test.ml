open Format

type initial_value = Constant of int64
                   | VirtualAddress of string
                   | PTE of string
                   | MkDesc of string


let pp_hex_constant fmt n =
  if n = Int64.zero then
    fprintf fmt "0x0"
  else
    fprintf fmt "%#Lx" n

let pp_initial_value fmt = function
  | Constant n -> fprintf fmt "extz(%a, 64)" pp_hex_constant n
  | VirtualAddress address -> fprintf fmt "%s" address
  | PTE va -> fprintf fmt "pte3(%s, page_table_base)" va
  | MkDesc pa -> fprintf fmt "mkdesc3(oa=%s)" pa

module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type thread =
  {
    setup : initial_value IntMap.t;
    instructions : Instruction.t list;
    handler : (int * string) option; (* TODO: fix this representation *)
  }

let new_thread = { setup = IntMap.empty; instructions = []; handler = None; }

module AssertionValue = struct
  type t = Constant of int64
         | PTE of string
end

type condition = RegisterAssertion of int * int * AssertionValue.t
               | LocationAssertion of string * int64

type t =
  {
    name: string;
    cycle: string;
    virtual_addresses : StringSet.t;
    physical_addresses : StringSet.t;
    initial_mappings : string StringMap.t;
    possible_mappings : string StringMap.t;
    threads : thread list;
    assertion : condition list;
  }

let pp_condition = function
  | RegisterAssertion (proc, reg, AssertionValue.Constant v) -> sprintf "%d:X%d = %Ld" proc reg v
  | RegisterAssertion (proc, reg, AssertionValue.PTE pa) -> sprintf "%d:X%d = mkdesc3(oa=%s)" proc reg pa
  | LocationAssertion (loc, v) -> sprintf "*%s = %Ld" loc v

let pp_assertion fmt = function
  | [] -> fprintf fmt "<empty>"
  | conditions -> fprintf fmt "%s" @@ String.concat " & " (List.map pp_condition conditions)

module Printer(Arch : Arch.Sig) = struct
  let pp_thread fmt index thread =
    fprintf fmt "[thread.%d]\n" index;
    fprintf fmt "code = \"\"\"\n";
    List.iter (fun instruction -> Arch.to_concrete_instructions instruction |> List.iter (fprintf fmt "    %a\n" Arch.print_asm)) thread.instructions;
    fprintf fmt "\"\"\"\n\n";

    fprintf fmt "[thread.%d.reset]\n" index;
    IntMap.iter (fun reg value -> fprintf fmt "R%d = \"%a\"\n" reg pp_initial_value value) thread.setup;
    fprintf fmt "\n";
    begin match thread.handler with
      | None -> ()
      | Some (addr, _) -> fprintf fmt "VBAR_EL1 = \"extz(%#x, 64)\"\n\n" (addr - 0x400)
    end;

    match thread.handler with
      | None -> ()
      | Some (addr, code) ->
        fprintf fmt "[section.thread%d_el1_handler]\n" index;
        fprintf fmt "address = \"%#x\"\n" addr;
        fprintf fmt "code = \"\"\"\n%s\"\"\"\n\n" code

  let print_types fmt test =
    let to_location = function
      | LocationAssertion (loc, _) -> Some loc
      | _ -> None in
    let locations = List.filter_map to_location test.assertion in
    if locations <> [] then begin
      fprintf fmt "[types]\n";
      List.iter (fprintf fmt "\"%s\" = \"uint64_t\"\n") locations;
      fprintf fmt "\n"
    end

  let pp_test fmt test =
    fprintf fmt "arch = \"%s\"\n" Arch.name;
    fprintf fmt "name = \"%s\"\n" test.name;
    fprintf fmt "orig = \"%s\"\n" test.cycle;
    fprintf fmt "symbolic = [%s]\n\n" (StringSet.elements test.virtual_addresses |> List.map (fun va -> "\"" ^ va ^ "\"") |> String.concat ", ");

    fprintf fmt "page_table_setup = \"\"\"\n";
    fprintf fmt "    physical %s;\n" (StringSet.elements test.physical_addresses |> String.concat " ");
    StringMap.iter (fprintf fmt "    %s |-> %s;\n") test.initial_mappings;
    StringMap.iter (fprintf fmt "    %s ?-> %s;\n") test.possible_mappings;
    StringSet.iter (fprintf fmt "    *%s = 0;\n") test.physical_addresses;
    List.iter (fun thread -> match thread.handler with | None -> () | Some (addr, _) -> fprintf fmt "    identity %#x with code;\n" (addr - 0x400)) test.threads;
    fprintf fmt "\"\"\"\n\n";

    print_types fmt test;

    List.iteri (pp_thread fmt) (List.rev test.threads);

    fprintf fmt "[final]\n";
    fprintf fmt "assertion = \"%a\"" pp_assertion test.assertion
end