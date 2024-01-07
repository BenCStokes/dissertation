let () = print_endline "Hello, World!"

open Cycle
open Gen

let cycle = [FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]

let () = List.iter (fun (x,y) -> Printf.printf "(%d, %d)\n" x y) (test_from_cycle cycle)

(*let () = let open Format in let open AArch64 in printf "%a" print_asm (LDR(Register 0,Register 1))*)