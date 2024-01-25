open Cycle
open Gen

let cycle =
  (*[FromRead (`Same, Internal); ProgramOrder (`Same, Write, Read)]*) (* CoWR? *)
  (*[FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]*) (* CoWR.alias *)
  [FromRead (`Aliased, External); ReadsFrom (`Same, External); ProgramOrder (`Aliased, Read, Read)] (* CoRR0.alias+po *)
  (*[FromRead (`Same, External); ReadsFrom (`Same, External); ProgramOrder (`Same, Read, Read)]*) (* CoRR0+po? *)
  (*[ProgramOrder (`Same, Write, Read); FromRead (`Same, External); WriteSerialisation (`Same, External)]*)

let test = test_from_cycle cycle

module TestPrinter = Test.Printer(AArch64)
let () = Format.printf "%a\n" TestPrinter.pp_test test