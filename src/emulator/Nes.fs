module NES

open ROM
open Mapper
open MEM
open CPU
open NESTEST

let trace (cpu: CPU.t) instruction opcode =
    let str_op = instruction.op.ToString()
    // printfn
    //     "%04X %s    A:%02X X:%02X Y:%02X P:%02X SP:%02X    CYC:%4d"
    //     cpu.pc
    //     str_op
    //     cpu.a
    //     cpu.x
    //     cpu.y
    //     (CPU.flags_to_int cpu)
    //     cpu.s
    //     cpu.cycles

    let s1 = sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" cpu.a cpu.x cpu.y (CPU.flags_to_int cpu) cpu.s
    // printfn "%s" reglog[cpu.steps+1]
    if not (s1.Equals(reglog[cpu.steps+1])) 
    then 
        // let s2 = sprintf "%04X %s    A:%02X X:%02X Y:%02X P:%02X SP:%02X    CYC:%4d" cpu.pc str_op cpu.a cpu.x cpu.y (CPU.flags_to_int cpu) cpu.s cpu.cycles
        printfn "\n           : %04X %s" cpu.pc str_op
        printfn "OK         : %s" reglog[cpu.steps+1]
        printfn "           : %s" s1
        failwith (sprintf "nestest error")

    
type Nes() =

    let rom = new Rom()
    let lromcop = rom.setRom "nestest.nes"
    let lmappr = Mapper.mapper_for lromcop
    let lmem = MEM.makeRam (lmappr)
    let mutable lcpu = CPU.make true true lmem

    member this.setRom(path: string) = printfn "Loaded %s\n" path

    member this.loopNes =
        let pcval = CPU.init  &lcpu true
        for i in 0..6000 do
            CPU.stepCpu &lcpu trace
