type location_flag = [`Same | `Diff | `Aliased]
type same_pa_location_flag = [`Same | `Aliased]

type event_type_flag = Read | Write

type processor_flag = Internal | External

type relation = ProgramOrder of location_flag * event_type_flag * event_type_flag
              | FromRead of same_pa_location_flag * processor_flag
              | ReadsFrom of same_pa_location_flag * processor_flag
              | WriteSerialisation of same_pa_location_flag * processor_flag
              | MakeTranslation of processor_flag
              (* | BreakTranslation of *)
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

let parse_relation s =
  if String.starts_with ~prefix:"Po" s then
    ProgramOrder (location_flag_of_char s.[2], event_type_flag_of_char s.[3], event_type_flag_of_char s.[4])
  else if String.starts_with ~prefix:"Fr" s then
    FromRead (same_pa_location_flag_of_char s.[2], processor_flag_of_char s.[3])
  else if String.starts_with ~prefix:"Rf" s then
    ReadsFrom (same_pa_location_flag_of_char s.[2], processor_flag_of_char s.[3])
  else if String.starts_with ~prefix:"Ws" s || String.starts_with ~prefix:"Co" s then
    WriteSerialisation (same_pa_location_flag_of_char s.[2], processor_flag_of_char s.[3])
  else if String.starts_with ~prefix:"Mt" s then
    MakeTranslation (processor_flag_of_char s.[2])
  else
    raise (Invalid_argument ("Invalid relation: " ^ s))

let parse s = String.split_on_char ' ' s |> List.map parse_relation

let lhs_type = function
  | ProgramOrder (_, lhs_type, _) -> lhs_type
  | FromRead _ -> Read
  | ReadsFrom _ -> Write
  | WriteSerialisation _ -> Write
  | MakeTranslation _ -> Write

let rhs_type = function
  | ProgramOrder (_, _, rhs_type) -> rhs_type
  | FromRead _ -> Write
  | ReadsFrom _ -> Read
  | WriteSerialisation _ -> Write
  | MakeTranslation _ -> Read

let processor_info = function
  | ProgramOrder (_, _, _) -> Internal
  | FromRead (_, info)
  | ReadsFrom (_, info)
  | WriteSerialisation (_, info)
  | MakeTranslation info -> info

let location_info = function
  | ProgramOrder (info, _, _) -> info
  | FromRead (info, _)
  | ReadsFrom (info, _)
  | WriteSerialisation (info, _) -> (info :> location_flag)
  | MakeTranslation _ -> `Diff

type t = relation list