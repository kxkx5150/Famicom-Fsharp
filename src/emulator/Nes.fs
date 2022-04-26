module NES

open ROM
open Mapper
open MEM
open CPU

let trace (cpu: CPU.t) instruction opcode =
    let cy = cpu.cycles * 3 % 341 in
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

type Nes() =

    let rom = new Rom()
    let lromcop = rom.setRom "nestest.nes"
    let lmappr = Mapper.mapper_for lromcop
    let lmem = MEM.makeRam (lmappr)
    let mutable lcpu = CPU.make true true lmem

    member this.setRom(path: string) = printfn "Loaded %s\n" path

    member this.loopNes =
        let pcval = CPU.init  &lcpu true
        for i in 0..2000 do
            CPU.stepCpu &lcpu trace
