type location_flag = [`Same | `Diff | `Aliased]
type same_pa_location_flag = [`Same | `Aliased]
type translation_location_flag = [`Same | `Diff] (* TODO: git blame this *)

type event_type_flag = Read | Write

type translation_write_flag = Make | Break

type processor_flag = Internal | External

type dependency_type = Control | Address | Data

type relation = ProgramOrder of location_flag * event_type_flag * event_type_flag
              | FromRead of same_pa_location_flag * processor_flag
              | ReadsFrom of same_pa_location_flag * processor_flag
              | WriteSerialisation of same_pa_location_flag * processor_flag
              | TranslationReadsFrom of processor_flag * translation_write_flag
              | TranslationFromRead of processor_flag * translation_write_flag
              | Barrier of location_flag * event_type_flag * event_type_flag * Instruction.t list
              | Dependency of dependency_type * location_flag * event_type_flag
              (* ... *)

let location_flag_of_char = function
  | 's' -> `Same
  | 'd' -> `Diff
  | 'a' -> `Aliased
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid location flag: %c" c))

let same_pa_location_flag_of_char = function
  | 'd' -> raise (Invalid_argument "Invalid use of 'd' location flag")
  | 's' -> `Same
  | 'a' -> `Aliased
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid location flag: %c" c))

let event_type_flag_of_char = function
  | 'R' -> Read
  | 'W' -> Write
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid event type flag: %c" c))

let processor_flag_of_char = function
  | 'i' -> Internal
  | 'e' -> External
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid processor flag: %c" c))

let translation_write_flag_of_char = function
  | 'm' -> Make
  | 'b' -> Break
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid translation write flag: %c" c))

let drop_prefix n str = String.sub str n (String.length str - n)
let drop_suffix n str = String.sub str 0 (String.length str - n)

let parse_barrier_spec spec = let open Instruction in
  if String.starts_with ~prefix:"DMB." spec then
    ArmDMBEquiv (drop_prefix 4 spec |> parse_barrier_param)
  else if String.starts_with ~prefix:"DSB." spec then
    ArmDSBEquiv (drop_prefix 4 spec |> parse_barrier_param)
  else if String.equal "ISB" spec then
    ArmISBEquiv
  else
    raise (Invalid_argument ("Invalid barrier specification: " ^ spec))

let is_barrier s =
  let rec go = function
    | [] -> false
    | prefix::prefixes -> String.starts_with ~prefix s || go prefixes in
  go ["DMB"; "DSB"; "ISB"]

let parse_dependency_type = function
  | "Addr" -> Address
  | "Ctrl" -> Control
  | "Data" -> Data
  | s -> raise (Invalid_argument ("Invalid dependency type: " ^ s))

let parse_relation s =
  if String.starts_with ~prefix:"Po" s then
    ProgramOrder (location_flag_of_char s.[2], event_type_flag_of_char s.[3], event_type_flag_of_char s.[4])
  else if String.starts_with ~prefix:"Fr" s then
    FromRead (same_pa_location_flag_of_char s.[2], processor_flag_of_char s.[3])
  else if String.starts_with ~prefix:"Rf" s then
    ReadsFrom (same_pa_location_flag_of_char s.[2], processor_flag_of_char s.[3])
  else if String.starts_with ~prefix:"Ws" s || String.starts_with ~prefix:"Co" s then
    WriteSerialisation (same_pa_location_flag_of_char s.[2], processor_flag_of_char s.[3])
  else if String.starts_with ~prefix:"Trf" s then
    TranslationReadsFrom (processor_flag_of_char s.[3], translation_write_flag_of_char s.[4])
  else if String.starts_with ~prefix:"Tfr" s then
    TranslationFromRead (processor_flag_of_char s.[3], translation_write_flag_of_char s.[4])
  else if is_barrier s then
    let specs = drop_suffix 3 s |> String.split_on_char '+' in
    let s = String.sub s (String.length s - 3) 3 in
    Barrier (location_flag_of_char s.[0], event_type_flag_of_char s.[1], event_type_flag_of_char s.[2], List.map parse_barrier_spec specs)
  else if String.starts_with ~prefix:"Dp" s then
    let dependency_type = parse_dependency_type (String.sub s 2 4) in
    Dependency (dependency_type, location_flag_of_char s.[6], event_type_flag_of_char s.[7])
  else
    raise (Invalid_argument ("Invalid relation: " ^ s))

let parse s = String.split_on_char ' ' s |> List.map parse_relation

let lhs_type = function
  | ProgramOrder (_, lhs_type, _) | Barrier (_, lhs_type, _, _) -> lhs_type
  | FromRead _ -> Read
  | ReadsFrom _ -> Write
  | WriteSerialisation _ -> Write
  | TranslationReadsFrom _ -> Write
  | TranslationFromRead _ -> Read
  | Dependency _ -> Read

let lhs_must_be_at_pte = function
  | TranslationReadsFrom _ -> true
  | _ -> false

let rhs_type = function
  | ProgramOrder (_, _, rhs_type) | Barrier (_, _, rhs_type, _) | Dependency (_, _, rhs_type) -> rhs_type
  | FromRead _ -> Write
  | ReadsFrom _ -> Read
  | WriteSerialisation _ -> Write
  | TranslationReadsFrom _ -> Read
  | TranslationFromRead _ -> Write

let rhs_must_be_at_pte = function
  | TranslationFromRead _ -> true
  | _ -> false

let processor_info = function
  | ProgramOrder _ | Barrier _ | Dependency _ -> Internal
  | FromRead (_, info)
  | ReadsFrom (_, info)
  | WriteSerialisation (_, info)
  | TranslationReadsFrom (info, _)
  | TranslationFromRead (info, _) -> info

let location_info = function
  | Dependency (_, info, _)
  | ProgramOrder (info, _, _) | Barrier (info, _, _, _) -> info
  | FromRead (info, _)
  | ReadsFrom (info, _)
  | WriteSerialisation (info, _) -> (info :> location_flag)
  | TranslationReadsFrom _ | TranslationFromRead _ -> `Same

type t = relation list