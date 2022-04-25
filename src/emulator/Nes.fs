module NES

open ROM
open Mapper
open MEM
open CPU

type Nes() =

    let rom = new Rom()
    let lromcop = rom.setRom "nestest.nes"
    let lmappr = Mapper.mapper_for lromcop
    let lmem = MEM.makeRam(lmappr)
    let lcpu = CPU.make false false lmem

    member this.romc = lromcop
    member this.mapper = lmappr
    member this.mem = lmem
    member this.cpu = lcpu

    member this.setRom(path: string) = 
        printfn "Loaded rom %s\n" path

    member this.step =
        printfn "step nes\n"
        CPU.step this.cpu


