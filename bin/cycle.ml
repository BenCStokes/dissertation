type location_flag = [`Same | `Diff | `Aliased]
type same_pa_location_flag = [`Same | `Aliased]
type translation_location_flag = [`Same | `Diff]

type event_type_flag = Read | Write | TranslationWrite

type translation_write_flag = Make | Break

type processor_flag = Internal | External

type relation = ProgramOrder of location_flag * event_type_flag * event_type_flag
              | FromRead of same_pa_location_flag * processor_flag
              | ReadsFrom of same_pa_location_flag * processor_flag
              | WriteSerialisation of same_pa_location_flag * processor_flag
              | TranslationReadsFrom of processor_flag * translation_write_flag
              | TranslationFromRead of processor_flag * translation_write_flag
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
  | 'T' -> TranslationWrite
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid event type flag: %c" c))

let processor_flag_of_char = function
  | 'i' -> Internal
  | 'e' -> External
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid processor flag: %c" c))

let translation_write_flag_of_char = function
  | 'm' -> Make
  | 'b' -> Break
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid translation write flag: %c" c))

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
  else
    raise (Invalid_argument ("Invalid relation: " ^ s))

let parse s = String.split_on_char ' ' s |> List.map parse_relation

let lhs_type = function
  | ProgramOrder (_, lhs_type, _) -> lhs_type
  | FromRead _ -> Read
  | ReadsFrom _ -> Write
  | WriteSerialisation _ -> Write
  | TranslationReadsFrom _ -> TranslationWrite
  | TranslationFromRead _ -> Read

let rhs_type = function
  | ProgramOrder (_, _, rhs_type) -> rhs_type
  | FromRead _ -> Write
  | ReadsFrom _ -> Read
  | WriteSerialisation _ -> Write
  | TranslationReadsFrom _ -> Read
  | TranslationFromRead _ -> TranslationWrite

let processor_info = function
  | ProgramOrder (_, _, _) -> Internal
  | FromRead (_, info)
  | ReadsFrom (_, info)
  | WriteSerialisation (_, info)
  | TranslationReadsFrom (info, _) -> info
  | TranslationFromRead (info, _) -> info

let location_info = function
  | ProgramOrder (info, _, _) -> info
  | FromRead (info, _)
  | ReadsFrom (info, _)
  | WriteSerialisation (info, _) -> (info :> location_flag)
  | TranslationReadsFrom _ | TranslationFromRead _ -> `Same

type t = relation list