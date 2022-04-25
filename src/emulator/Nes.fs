module NES

open ROM
open Mapper
open MEM
open CPU

let trace (cpu: CPU.t) instruction opcode =
    let cy = cpu.cycles * 3 % 341 in
    let str_op = instruction.op.ToString()

    printfn
        "%04X %s    A:%02X X:%02X Y:%02X P:%02X SP:%02X    CYC:%3d"
        cpu.pc
        str_op
        cpu.a
        cpu.x
        cpu.y
        (CPU.flags_to_int cpu)
        cpu.s
        cy

type Nes() =

    let rom = new Rom()
    let lromcop = rom.setRom "nestest.nes"
    let lmappr = Mapper.mapper_for lromcop
    let lmem = MEM.makeRam (lmappr)
    let lcpu = CPU.make false false lmem

    member this.romc = lromcop
    member this.mapper = lmappr
    member this.mem = lmem
    member this.cpu = lcpu

    member this.setRom(path: string) = printfn "Loaded %s\n" path

    member this.loop =
        for i in 0..9 do
            CPU.step this.cpu trace
