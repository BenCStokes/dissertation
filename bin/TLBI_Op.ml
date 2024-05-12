(* This file was generated from a script, given the list of 32 TLBI operations *)

type t = ALLE1
       | ALLE1IS
       | ALLE2
       | ALLE2IS
       | ALLE3
       | ALLE3IS
       | ASIDE1
       | ASIDE1IS
       | IPAS2E1
       | IPAS2E1IS
       | IPAS2LE1
       | IPAS2LE1IS
       | VAAE1
       | VAAE1IS
       | VAALE1
       | VAALE1IS
       | VAE1
       | VAE1IS
       | VAE2
       | VAE2IS
       | VAE3
       | VAE3IS
       | VALE1
       | VALE1IS
       | VALE2
       | VALE2IS
       | VALE3
       | VALE3IS
       | VMALLE1
       | VMALLE1IS
       | VMALLS12E1
       | VMALLS12E1IS

let parse = function
  | "ALLE1" -> ALLE1
  | "ALLE1IS" -> ALLE1IS
  | "ALLE2" -> ALLE2
  | "ALLE2IS" -> ALLE2IS
  | "ALLE3" -> ALLE3
  | "ALLE3IS" -> ALLE3IS
  | "ASIDE1" -> ASIDE1
  | "ASIDE1IS" -> ASIDE1IS
  | "IPAS2E1" -> IPAS2E1
  | "IPAS2E1IS" -> IPAS2E1IS
  | "IPAS2LE1" -> IPAS2LE1
  | "IPAS2LE1IS" -> IPAS2LE1IS
  | "VAAE1" -> VAAE1
  | "VAAE1IS" -> VAAE1IS
  | "VAALE1" -> VAALE1
  | "VAALE1IS" -> VAALE1IS
  | "VAE1" -> VAE1
  | "VAE1IS" -> VAE1IS
  | "VAE2" -> VAE2
  | "VAE2IS" -> VAE2IS
  | "VAE3" -> VAE3
  | "VAE3IS" -> VAE3IS
  | "VALE1" -> VALE1
  | "VALE1IS" -> VALE1IS
  | "VALE2" -> VALE2
  | "VALE2IS" -> VALE2IS
  | "VALE3" -> VALE3
  | "VALE3IS" -> VALE3IS
  | "VMALLE1" -> VMALLE1
  | "VMALLE1IS" -> VMALLE1IS
  | "VMALLS12E1" -> VMALLS12E1
  | "VMALLS12E1IS" -> VMALLS12E1IS
  | s -> raise (Invalid_argument ("Invalid TLBI operation: " ^ s))

let to_string = function
  | ALLE1 -> "ALLE1"
  | ALLE1IS -> "ALLE1IS"
  | ALLE2 -> "ALLE2"
  | ALLE2IS -> "ALLE2IS"
  | ALLE3 -> "ALLE3"
  | ALLE3IS -> "ALLE3IS"
  | ASIDE1 -> "ASIDE1"
  | ASIDE1IS -> "ASIDE1IS"
  | IPAS2E1 -> "IPAS2E1"
  | IPAS2E1IS -> "IPAS2E1IS"
  | IPAS2LE1 -> "IPAS2LE1"
  | IPAS2LE1IS -> "IPAS2LE1IS"
  | VAAE1 -> "VAAE1"
  | VAAE1IS -> "VAAE1IS"
  | VAALE1 -> "VAALE1"
  | VAALE1IS -> "VAALE1IS"
  | VAE1 -> "VAE1"
  | VAE1IS -> "VAE1IS"
  | VAE2 -> "VAE2"
  | VAE2IS -> "VAE2IS"
  | VAE3 -> "VAE3"
  | VAE3IS -> "VAE3IS"
  | VALE1 -> "VALE1"
  | VALE1IS -> "VALE1IS"
  | VALE2 -> "VALE2"
  | VALE2IS -> "VALE2IS"
  | VALE3 -> "VALE3"
  | VALE3IS -> "VALE3IS"
  | VMALLE1 -> "VMALLE1"
  | VMALLE1IS -> "VMALLE1IS"
  | VMALLS12E1 -> "VMALLS12E1"
  | VMALLS12E1IS -> "VMALLS12E1IS"