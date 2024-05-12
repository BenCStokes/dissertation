type location_flag = [`Same | `Diff | `Aliased]
type same_pa_location_flag = [`Same | `Aliased]
type translation_location_flag = [`Same | `Diff] (* TODO: git blame this *)

type event_type_flag = Read | Write | TLBI of TLBI_Op.t

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
  let parse p tl s f =
    Option.bind (p s) @@ fun (s', tok) ->
    tl s' (f tok) in
  let stop = function
    | "" -> Option.some
    | _ -> fun _ -> None in
  let rest s = Some ("", s) in
  let (=>) = Fun.flip in
  let (<|>) f g s = match f s with | None -> g s | some -> some in
  let (<<|>>) f g parser s h = ((f parser => h) <|> (g parser => h)) s in
  let skip prefix parser s f =
    if String.starts_with ~prefix s then
      parser (drop_prefix (String.length prefix) s) f
    else
      None in
  let from_char f s =
    if s = "" then None else
      Option.map (fun flag -> (drop_prefix 1 s, flag)) (f s.[0]) in
  let location_flag = from_char (function
    | 's' -> Some `Same
    | 'd' -> Some `Diff
    | 'a' -> Some `Aliased
    | _ -> None) in
  let same_pa_location_flag = from_char (function
    | 's' -> Some `Same
    | 'a' -> Some `Aliased
    | _ -> None) in
  let event_type_flag s =
    if String.starts_with ~prefix:"R" s then
      Some (drop_prefix 1 s, Read)
    else if String.starts_with ~prefix:"W" s then
      Some (drop_prefix 1 s, Write)
    else if String.starts_with ~prefix:"T(" s then
      let s = drop_prefix 2 s in
      Option.bind (String.index_opt s ')') (fun i ->
        let op = try Some (TLBI_Op.parse (String.sub s 0 i)) with Invalid_argument _ -> None in
        Option.map (fun op -> (drop_prefix (i+1) s, TLBI op)) op)
    else None in
  (* let event_type_flag = from_char (function
    | 'R' -> Some Read
    | 'W' -> Some Write
    | _ -> None) in *)
  let processor_flag = from_char (function
    | 'i' -> Some Internal
    | 'e' -> Some External
    | _ -> None) in
  let same_pa_location_flag_or_same s = match same_pa_location_flag s with
    | None -> Some (s, `Same)
    | some -> some in
  let translation_write_flag = from_char (function
    | 'b' -> Some Break
    | 'm' -> Some Make
    | _ -> None) in
  let rec barriers s =
    let open Instruction in
    let barrier s =
      if String.starts_with ~prefix:"ISB" s then
        Some (drop_prefix 3 s, ArmISBEquiv)
      else if String.starts_with ~prefix:"DSB." s then
        let s = drop_prefix 4 s in
        let param = try Some (parse_barrier_param (String.sub s 0 2)) with Invalid_argument _ -> None in
        Option.map (fun param -> drop_prefix 2 s, ArmDSBEquiv param) param
      else if String.starts_with ~prefix:"DMB." s then
        let s = drop_prefix 4 s in
        let param = try Some (parse_barrier_param (String.sub s 0 2)) with Invalid_argument _ -> None in
        Option.map (fun param -> drop_prefix 2 s, ArmDMBEquiv param) param
      else None in
    begin 
      begin
        parse barrier @@ skip "+" @@ parse barriers @@ parse rest @@ stop => fun b bs rest -> (rest, b::bs)
      end <|> begin 
        parse barrier @@ parse rest @@ stop => fun b rest -> (rest, [b])
      end
    end s in
  let dependency_type s =
    let out = try Some (parse_dependency_type (String.sub s 0 4)) with Invalid_argument _ -> None in
    Option.map (fun out -> drop_prefix 4 s, out) out in
  begin
    begin
      skip "Po" @@ parse location_flag @@ parse event_type_flag @@ parse event_type_flag @@ stop
        => fun loc e1 e2 -> ProgramOrder (loc, e1, e2)
    end <|> begin
      skip "Fr" @@ parse same_pa_location_flag_or_same @@ parse processor_flag @@ stop
        => fun loc proc -> FromRead (loc, proc)
    end <|> begin
      skip "Rf" @@ parse same_pa_location_flag_or_same @@ parse processor_flag @@ stop
        => fun loc proc -> ReadsFrom (loc, proc)
    end <|> begin
      (skip "Co" <<|>> skip "Ws") @@ parse same_pa_location_flag_or_same @@ parse processor_flag @@ stop
        => fun loc proc -> WriteSerialisation (loc, proc)
    end <|> begin
      skip "Trf" @@ parse processor_flag @@ parse translation_write_flag @@ stop
        => fun proc t -> TranslationReadsFrom (proc, t)
    end <|> begin
      skip "Tfr" @@ parse processor_flag @@ parse translation_write_flag @@ stop
        => fun proc t -> TranslationFromRead (proc, t)
    end <|> begin
      parse barriers @@ parse location_flag @@ parse event_type_flag @@ parse event_type_flag @@ stop
        => fun bs loc e1 e2 -> Barrier (loc, e1, e2, bs)
    end <|> begin
      skip "Dp" @@ parse dependency_type @@ parse location_flag @@ parse event_type_flag @@ stop
        => fun dt loc e -> Dependency (dt, loc, e)
    end
  end s |> function | None -> raise (Invalid_argument ("Invalid relation: " ^ s)) | Some rel -> rel

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