module NES

open ROM
open Mapper0
open MEM
open CPU
open NESTEST
open PPU

let trace (cpu: CPU.t) instruction opcode =
    let str_op = instruction.op.ToString()
    printfn
        "%04X %s    A:%02X X:%02X Y:%02X P:%02X SP:%02X    CYC:%4d"
        cpu.pc
        str_op
        cpu.a
        cpu.x
        cpu.y
        (CPU.flags_to_int cpu)
        cpu.s
        cpu.cycles
    if cpu.nestest then
        let s1 =
            sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" cpu.a cpu.x cpu.y (CPU.flags_to_int cpu) cpu.s
        let pcstr = sprintf "%04X" cpu.pc
        if
            not (s1.Equals(reglog[cpu.steps + 1]))
            || pcstr = ""
        then
            printfn "\n           : %04X %s" cpu.pc str_op
            printfn "OK         : %s" reglog[cpu.steps + 1]
            printfn "           : %s" s1
            failwith (sprintf "nestest error")

type Nes() =
    let rom = new Rom()
    let ppu = new PPU()
    let mapper = new Mapper0(rom, ppu)
    let mem = MEM.makeRam mapper


    let mutable lcpu = CPU.make false false mem
    let _ = mem.mapper.setRom "sm.nes"

    member this.initNes = 
        let pcval = CPU.init &lcpu
        ()

    member this.runNes =
        let prev_cycles = lcpu.cycles
        CPU.stepCpu &lcpu trace
        let elapsed_cycles = lcpu.cycles - prev_cycles
        mem.mapper.runPpu elapsed_cycles


